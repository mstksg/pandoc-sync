{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Text.Pandoc.Sync.Writer (
    writePandoc
  ) where

-- import qualified Text.Pandoc.MediaBag      as P
-- import qualified Text.Pandoc.Readers.LaTeX as P
import           Control.Applicative
import           Control.Exception
import           Control.Lens hiding          ((<.>), (%~))
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Data.Default
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Singletons
import           Data.Singletons.Decide
import           Data.Singletons.Prelude.Bool
import           System.Directory
import           System.FilePath
import           System.IO.Error
import           Text.Pandoc.Sync.Format
import qualified Data.ByteString.Lazy         as B
import qualified Data.Text.Lazy.Encoding      as TL
import qualified Data.Text.Lazy.IO            as TL
import qualified Text.Pandoc                  as P
import qualified Text.Pandoc.Lens.App         as P
import qualified Text.Pandoc.MediaBag         as P
import qualified Text.Pandoc.Options          as P
import qualified Text.Pandoc.PDF              as P
import qualified Text.Pandoc.SelfContained    as P
import qualified Text.Pandoc.Shared           as P
import qualified Text.Pandoc.Templates        as P
import qualified Text.Pandoc.UTF8             as UTF8

writePandoc
    :: SingI r
    => Format r 'True
    -> P.Pandoc
    -> P.MediaBag
    -> WriterOptions
    -> FilePath
    -> IO ()
writePandoc ft pd bag wo fp = do
    wo' <- mkWriterOptions ft wo bag
    case formatWriter ft of
      P.IOStringWriter f ->
        UTF8.writeFile fp                =<< f wo' pd
      P.IOByteStringWriter f ->
        B.writeFile (UTF8.encodePath fp) =<< f wo' pd
      P.PureStringWriter f -> case ft of
        FPDF pdft -> do
          let eng = pdfEngine pdft
          -- TODO: handle lack of prog?
          mbPdfProg <- findExecutable eng
          print $ has _Just mbPdfProg
          res <- P.makePDF eng f wo' pd
          putStrLn $ "hey pdf! " ++ fp
          -- TODO: handle bad res?
          case res of
            Right res' -> B.writeFile (UTF8.encodePath fp) res'
            Left  err  -> do
              putStrLn "Failed pdf?"
              TL.putStrLn . TL.decodeUtf8 $ err
              -- B.writeFile (UTF8.encodePath fp) err
        _ -> do
            let res = f wo' pd
            out <- if htmlFormat ft
               then P.makeSelfContained wo' res
               else return res
            UTF8.writeFile fp out

mkWriterOptions
    :: forall r. SingI r
    => Format r 'True
    -> WriterOptions
    -> P.MediaBag
    -> IO P.WriterOptions
mkWriterOptions ft wo bag = do
    datadir <- runMaybeT $
          MaybeT (return $ wo ^. woDataDir)
      <|> MaybeT (catch @SomeException
                        (Just <$> getAppUserDataDirectory "pandoc")
                        (\_ -> return Nothing                     )
                 )

    templ <- case wo ^. woTemplatePath of
      _ | not standalone -> return Nothing
      Nothing ->
        either throwIO (return . Just) =<< P.getDefaultTemplate datadir (writerString ft)
      Just tp -> do
        -- strip off extensions
        let tp' = case takeExtension tp of
                    ""   -> tp <.> writerString ft
                    _    -> tp
        Just <$> catch (UTF8.readFile tp')
          (\e -> if isDoesNotExistError e
                    then catch @SomeException
                               (P.readDataFileUTF8 datadir ("templates" </> tp'))
                               throwIO
                    else throwIO e
          )

    mathVar <- forM (wo ^? woMathMethod . P._LaTeXMathML . _Nothing) $ \_ -> do
       s <- P.readDataFileUTF8 datadir "LaTeXMathML.js"
       return ("mathml-script", s)
    dzVar   <- forM (ft ^? asReader SFalse . _FSlideShow . _SSDZSlides) $ \_ -> do
       dztempl <- P.readDataFileUTF8 datadir ("dzslides" </> "template.html")
       let dzline = "<!-- {{{{ dzslides core"
           dzcore = unlines
                  $ dropWhile (not . (dzline `isPrefixOf`))
                  $ lines dztempl
       return ("dzslides-core", dzcore)
    let variables = concat [ maybeToList mathVar
                           , maybeToList dzVar
                           , wo ^. woVariables
                           ]

    return def { P.writerTemplate        = templ
               , P.writerVariables       = variables
               , P.writerTabStop         = wo ^. woTabStop      -- can we match output?
               , P.writerTableOfContents = wo ^. woTOC
               , P.writerHTMLMathMethod  = wo ^. woMathMethod
               , P.writerHtml5           = getAny $ ft ^. asReader STrue . _FHTML . _Unwrapped
               , P.writerBeamer          = False        -- it's false in the actual thing
               -- , P.writerBeamer          = has (asReader SFalse . _FSlideShow . _SSBeamer) ft
               , P.writerMediaBag        = bag
               }

  where
    standalone = wo ^. woStandalone
              || not (isTextFormat ft)
              || case ft of FPDF _ -> True; _ -> False

--   let writerOptions = def { writerIncremental      = incremental,
--                             writerCiteMethod       = citeMethod,
--                             writerIgnoreNotes      = False,
--                             writerNumberSections   = numberSections,
--                             writerNumberOffset     = numberFrom,
--                             writerSectionDivs      = sectionDivs,
--                             writerReferenceLinks   = referenceLinks,
--                             writerReferenceLocation = referenceLocation,
--                             writerDpi              = dpi,
--                             writerWrapText         = wrap,
--                             writerColumns          = columns,
--                             writerEmailObfuscation = obfuscationMethod,
--                             writerIdentifierPrefix = idPrefix,
--                             writerSourceURL        = sourceURL,
--                             writerUserDataDir      = datadir,
--                             writerHtmlQTags        = htmlQTags,
--                             writerTopLevelDivision = topLevelDivision,
--                             writerListings         = listings,
--                             writerSlideLevel       = slideLevel,
--                             writerHighlight        = highlight,
--                             writerHighlightStyle   = highlightStyle,
--                             writerSetextHeaders    = setextHeaders,
--                             writerTeXLigatures     = texLigatures,
--                             writerEpubMetadata     = epubMetadata,
--                             writerEpubStylesheet   = epubStylesheet,
--                             writerEpubFonts        = epubFonts,
--                             writerEpubChapterLevel = epubChapterLevel,
--                             writerTOCDepth         = epubTOCDepth,
--                             writerReferenceODT     = referenceODT,
--                             writerReferenceDocx    = referenceDocx,
--                             writerVerbose          = verbose,
--                             writerLaTeXArgs        = latexEngineArgs
--                           }

isTextFormat :: Format r 'True -> Bool
isTextFormat = \case
    FODT    -> True
    FDocX   -> True
    FEPub _ -> True
    _       -> False

-- TODO: customizable latex engine
pdfEngine
    :: PDFType
    -> String
pdfEngine = \case
    PTLaTeX   -> "pdflatex"
    PTBeamer  -> "pdflatex"
    PTContext -> "context"
    PTHTML5   -> "wkhtmltopdf"

htmlFormat
    :: Format r w
    -> Bool
htmlFormat = \case
    FHTML _ -> True
    FSlideShow ss -> case ss of
      SSBeamer -> False
      _        -> True
    _       -> False

-- convertWithOpts :: Opt -> [FilePath] -> IO ()
-- convertWithOpts opts args = do
--   let Opt    {  optTabStop               = tabStop
--               , optPreserveTabs          = preserveTabs
--               , optStandalone            = standalone
--               , optReader                = readerName
--               , optWriter                = writerName
--               , optParseRaw              = parseRaw
--               , optVariables             = variables
--               , optMetadata              = metadata
--               , optTableOfContents       = toc
--               , optTransforms            = transforms
--               , optTemplate              = templatePath
--               , optOutputFile            = outputFile
--               , optNumberSections        = numberSections
--               , optNumberOffset          = numberFrom
--               , optSectionDivs           = sectionDivs
--               , optIncremental           = incremental
--               , optSelfContained         = selfContained
--               , optSmart                 = smart
--               , optOldDashes             = oldDashes
--               , optHtml5                 = html5
--               , optHtmlQTags             = htmlQTags
--               , optHighlight             = highlight
--               , optHighlightStyle        = highlightStyle
--               , optTopLevelDivision      = topLevelDivision
--               , optHTMLMathMethod        = mathMethod'
--               , optReferenceODT          = referenceODT
--               , optReferenceDocx         = referenceDocx
--               , optEpubStylesheet        = epubStylesheet
--               , optEpubMetadata          = epubMetadata
--               , optEpubFonts             = epubFonts
--               , optEpubChapterLevel      = epubChapterLevel
--               , optTOCDepth              = epubTOCDepth
--               , optDumpArgs              = dumpArgs
--               , optIgnoreArgs            = ignoreArgs
--               , optVerbose               = verbose
--               , optReferenceLinks        = referenceLinks
--               , optReferenceLocation     = referenceLocation
--               , optDpi                   = dpi
--               , optWrapText              = wrap
--               , optColumns               = columns
--               , optFilters               = filters
--               , optEmailObfuscation      = obfuscationMethod
--               , optIdentifierPrefix      = idPrefix
--               , optIndentedCodeClasses   = codeBlockClasses
--               , optDataDir               = mbDataDir
--               , optCiteMethod            = citeMethod
--               , optListings              = listings
--               , optLaTeXEngine           = latexEngine
--               , optLaTeXEngineArgs       = latexEngineArgs
--               , optSlideLevel            = slideLevel
--               , optSetextHeaders         = setextHeaders
--               , optAscii                 = ascii
--               , optTeXLigatures          = texLigatures
--               , optDefaultImageExtension = defaultImageExtension
--               , optExtractMedia          = mbExtractMedia
--               , optTrace                 = trace
--               , optTrackChanges          = trackChanges
--               , optFileScope            = fileScope
--               , optKaTeXStylesheet       = katexStylesheet
--               , optKaTeXJS               = katexJS
--              } = opts

--   when dumpArgs $
--     do UTF8.hPutStrLn stdout outputFile
--        mapM_ (UTF8.hPutStrLn stdout) args
--        exitSuccess

--   let csscdn = "https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.6.0/katex.min.css"
--   let mathMethod =
--         case (katexJS, katexStylesheet) of
--             (Nothing, _) -> mathMethod'
--             (Just js, ss) -> KaTeX js (fromMaybe csscdn ss)


--   -- --bibliography implies -F pandoc-citeproc for backwards compatibility:
--   let needsCiteproc = isJust (M.lookup "bibliography" (optMetadata opts)) &&
--                       optCiteMethod opts `notElem` [Natbib, Biblatex] &&
--                       "pandoc-citeproc" `notElem` map takeBaseName filters
--   let filters' = if needsCiteproc then "pandoc-citeproc" : filters
--                                   else filters

--   let sources = if ignoreArgs then [] else args

--   datadir <- case mbDataDir of
--                   Nothing   -> E.catch
--                                  (Just <$> getAppUserDataDirectory "pandoc")
--                                  (\e -> let _ = (e :: E.SomeException)
--                                         in  return Nothing)
--                   Just _    -> return mbDataDir

--   -- assign reader and writer based on options and filenames
--   let readerName' = case map toLower readerName of
--                           []       -> defaultReaderName
--                                       (if any isURI sources
--                                           then "html"
--                                           else "markdown") sources
--                           "html4"  -> "html"
--                           x        -> x

--   let writerName' = case map toLower writerName of
--                           []        -> defaultWriterName outputFile
--                           "epub2"   -> "epub"
--                           "html4"   -> "html"
--                           x         -> x
--   let format = takeWhile (`notElem` ['+','-'])
--                        $ takeFileName writerName'  -- in case path to lua script

--   let pdfOutput = map toLower (takeExtension outputFile) == ".pdf"

--   let laTeXOutput = format `elem` ["latex", "beamer"]
--   let conTeXtOutput = format == "context"
--   let html5Output = format == "html5"

--   let laTeXInput = "latex" `isPrefixOf` readerName' ||
--                     "beamer" `isPrefixOf` readerName'

--   writer <- if ".lua" `isSuffixOf` format
--                -- note:  use non-lowercased version writerName
--                then return $ IOStringWriter $ writeCustom writerName
--                else case getWriter writerName' of
--                          Left e  -> err 9 $
--                            if format == "pdf"
--                               then e ++
--                                "\nTo create a pdf with pandoc, use " ++
--                                "the latex or beamer writer and specify\n" ++
--                                "an output file with .pdf extension " ++
--                                "(pandoc -t latex -o filename.pdf)."
--                               else e
--                          Right w -> return w

--   reader <- if "t2t" == readerName'
--               then (mkStringReader .
--                     readTxt2Tags) <$>
--                       getT2TMeta sources outputFile
--               else case getReader readerName' of
--                 Right r  -> return r
--                 Left e   -> err 7 e'
--                   where e' = case readerName' of
--                                   "pdf" -> e ++
--                                      "\nPandoc can convert to PDF, but not from PDF."
--                                   "doc" -> e ++
--                                      "\nPandoc can convert from DOCX, but not from DOC.\nTry using Word to save your DOC file as DOCX, and convert that with pandoc."
--                                   _ -> e

--   let standalone' = standalone || not (isTextFormat format) || pdfOutput

--   templ <- case templatePath of
--                 _ | not standalone' -> return Nothing
--                 Nothing -> do
--                            deftemp <- getDefaultTemplate datadir format
--                            case deftemp of
--                                  Left e   -> throwIO e
--                                  Right t  -> return (Just t)
--                 Just tp -> do
--                            -- strip off extensions
--                            let tp' = case takeExtension tp of
--                                           ""   -> tp <.> format
--                                           _    -> tp
--                            Just <$> E.catch (UTF8.readFile tp')
--                              (\e -> if isDoesNotExistError e
--                                        then E.catch
--                                              (readDataFileUTF8 datadir
--                                                 ("templates" </> tp'))
--                                              (\e' -> let _ = (e' :: E.SomeException)
--                                                      in throwIO e')
--                                        else throwIO e)

--   variables' <- case mathMethod of
--                       LaTeXMathML Nothing -> do
--                          s <- readDataFileUTF8 datadir "LaTeXMathML.js"
--                          return $ ("mathml-script", s) : variables
--                       _ -> return variables

--   variables'' <- if format == "dzslides"
--                     then do
--                         dztempl <- readDataFileUTF8 datadir
--                                      ("dzslides" </> "template.html")
--                         let dzline = "<!-- {{{{ dzslides core"
--                         let dzcore = unlines
--                                    $ dropWhile (not . (dzline `isPrefixOf`))
--                                    $ lines dztempl
--                         return $ ("dzslides-core", dzcore) : variables'
--                     else return variables'

--   let sourceURL = case sources of
--                     []    -> Nothing
--                     (x:_) -> case parseURI x of
--                                 Just u
--                                   | uriScheme u `elem` ["http:","https:"] ->
--                                       Just $ show u{ uriQuery = "",
--                                                      uriFragment = "" }
--                                 _ -> Nothing

--   let readerOpts = def{ readerSmart = if laTeXInput
--                                          then texLigatures
--                                          else smart || (texLigatures &&
--                                                (laTeXOutput || conTeXtOutput))
--                       , readerStandalone = standalone'
--                       , readerParseRaw = parseRaw
--                       , readerColumns = columns
--                       , readerTabStop = tabStop
--                       , readerOldDashes = oldDashes
--                       , readerIndentedCodeClasses = codeBlockClasses
--                       , readerApplyMacros = not laTeXOutput
--                       , readerDefaultImageExtension = defaultImageExtension
--                       , readerTrace = trace
--                       , readerTrackChanges = trackChanges
--                       , readerFileScope   = fileScope
--                       }

-- #ifdef _WINDOWS
--   let istty = True
-- #else
--   istty <- queryTerminal stdOutput
-- #endif
--   when (istty && not (isTextFormat format) && outputFile == "-") $
--     err 5 $ "Cannot write " ++ format ++ " output to stdout.\n" ++
--             "Specify an output file using the -o option."

--   let readSources [] = mapM readSource ["-"]
--       readSources srcs = mapM readSource srcs
--       readSource "-" = UTF8.getContents
--       readSource src = case parseURI src of
--                             Just u | uriScheme u `elem` ["http:","https:"] ->
--                                        readURI src
--                                    | uriScheme u == "file:" ->
--                                        UTF8.readFile (uriPath u)
--                             _       -> UTF8.readFile src
--       readURI src = do
--         res <- openURL src
--         case res of
--              Left e        -> throwIO e
--              Right (bs,_)  -> return $ UTF8.toString bs

--   let readFiles [] = error "Cannot read archive from stdin"
--       readFiles [x] = B.readFile x
--       readFiles (x:xs) = mapM_ (warn . ("Ignoring: " ++)) xs >> B.readFile x

--   let convertTabs = tabFilter (if preserveTabs || readerName' == "t2t"
--                                  then 0
--                                  else tabStop)

--   let handleIncludes' :: String -> IO (Either PandocError String)
--       handleIncludes' = if readerName' `elem`  ["latex", "latex+lhs"]
--                                then handleIncludes
--                                else return . Right

--   let sourceToDoc :: [FilePath] -> IO (Pandoc, MediaBag)
--       sourceToDoc sources' = fmap handleError $
--         case reader of
--           StringReader r-> do
--             srcs <- convertTabs . intercalate "\n" <$> readSources sources'
--             doc <- handleIncludes' srcs
--             either (return . Left) (\s -> fmap (,mempty) <$> r readerOpts s) doc
--           ByteStringReader r -> readFiles sources' >>= r readerOpts

--   -- We parse first if (1) fileScope is set, (2), it's a binary
--   -- reader, or (3) we're reading JSON. This is easier to do of an AND
--   -- of negatives as opposed to an OR of positives, so we do default
--   -- parsing if it's a StringReader AND (fileScope is set AND it's not
--   -- a JSON reader).
--   (doc, media) <- case reader of
--     (StringReader _) | not fileScope && readerName' /= "json" ->
--                          sourceToDoc sources
--     _ | null sources -> sourceToDoc sources
--     _  -> do pairs <- mapM (\s -> sourceToDoc [s]) sources
--              return (mconcat $ map fst pairs, mconcat $ map snd pairs)

--   let writerOptions = def { writerTemplate         = templ,
--                             writerVariables        = variables'',
--                             writerTabStop          = tabStop,
--                             writerTableOfContents  = toc,
--                             writerHTMLMathMethod   = mathMethod,
--                             writerIncremental      = incremental,
--                             writerCiteMethod       = citeMethod,
--                             writerIgnoreNotes      = False,
--                             writerNumberSections   = numberSections,
--                             writerNumberOffset     = numberFrom,
--                             writerSectionDivs      = sectionDivs,
--                             writerReferenceLinks   = referenceLinks,
--                             writerReferenceLocation = referenceLocation,
--                             writerDpi              = dpi,
--                             writerWrapText         = wrap,
--                             writerColumns          = columns,
--                             writerEmailObfuscation = obfuscationMethod,
--                             writerIdentifierPrefix = idPrefix,
--                             writerSourceURL        = sourceURL,
--                             writerUserDataDir      = datadir,
--                             writerHtml5            = html5,
--                             writerHtmlQTags        = htmlQTags,
--                             writerTopLevelDivision = topLevelDivision,
--                             writerListings         = listings,
--                             writerBeamer           = False,
--                             writerSlideLevel       = slideLevel,
--                             writerHighlight        = highlight,
--                             writerHighlightStyle   = highlightStyle,
--                             writerSetextHeaders    = setextHeaders,
--                             writerTeXLigatures     = texLigatures,
--                             writerEpubMetadata     = epubMetadata,
--                             writerEpubStylesheet   = epubStylesheet,
--                             writerEpubFonts        = epubFonts,
--                             writerEpubChapterLevel = epubChapterLevel,
--                             writerTOCDepth         = epubTOCDepth,
--                             writerReferenceODT     = referenceODT,
--                             writerReferenceDocx    = referenceDocx,
--                             writerMediaBag         = media,
--                             writerVerbose          = verbose,
--                             writerLaTeXArgs        = latexEngineArgs
--                           }


--   doc' <- (maybe return (extractMedia media) mbExtractMedia >=>
--            adjustMetadata metadata >=>
--            applyTransforms transforms >=>
--            applyFilters datadir filters' [format]) doc

--   let writeFnBinary :: FilePath -> B.ByteString -> IO ()
--       writeFnBinary "-" = B.putStr
--       writeFnBinary f   = B.writeFile (UTF8.encodePath f)

--   let writerFn :: FilePath -> String -> IO ()
--       writerFn "-" = UTF8.putStr
--       writerFn f   = UTF8.writeFile f

--   case writer of
--     IOStringWriter f -> f writerOptions doc' >>= writerFn outputFile
--     IOByteStringWriter f -> f writerOptions doc' >>= writeFnBinary outputFile
--     PureStringWriter f
--       | pdfOutput -> do
--               -- make sure writer is latex or beamer or context or html5
--               unless (laTeXOutput || conTeXtOutput || html5Output) $
--                 err 47 $ "cannot produce pdf output with " ++ format ++
--                          " writer"

--               let pdfprog = case () of
--                               _ | conTeXtOutput -> "context"
--                               _ | html5Output   -> "wkhtmltopdf"
--                               _                 -> latexEngine
--               -- check for pdf creating program
--               mbPdfProg <- findExecutable pdfprog
--               when (isNothing mbPdfProg) $
--                    err 41 $ pdfprog ++ " not found. " ++
--                      pdfprog ++ " is needed for pdf output."

--               res <- makePDF pdfprog f writerOptions doc'
--               case res of
--                    Right pdf -> writeFnBinary outputFile pdf
--                    Left err' -> do
--                      B.hPutStr stderr err'
--                      B.hPut stderr $ B.pack [10]
--                      err 43 "Error producing PDF"
--       | otherwise -> selfcontain (f writerOptions doc' ++
--                                   ['\n' | not standalone'])
--                       >>= writerFn outputFile . handleEntities
--           where htmlFormat = format `elem`
--                   ["html","html5","s5","slidy","slideous","dzslides","revealjs"]
--                 selfcontain = if selfContained && htmlFormat
--                                  then makeSelfContained writerOptions
--                                  else return
--                 handleEntities = if htmlFormat && ascii
--                                     then toEntities
--                                     else id

_Proved :: Prism' (Decision a) a
_Proved = prism Proved (\case Proved x -> Right x
                              d        -> Left d
                       )
