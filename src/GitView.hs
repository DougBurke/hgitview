{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import qualified Brick as B
import qualified Brick.Widgets.List as WL
-- import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V

-- import qualified PackageInfo_hgitview as P
import qualified Paths_hgitview as P

import qualified System.Info as SI

import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Core ((<+>), hBox, vBox)

import Control.Monad (void, when)

import Data.List (intercalate, sortOn)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Tagged (Tagged(..))
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.LocalTime (TimeOfDay(..), timeToDaysAndTimeOfDay,
                            zonedTimeToUTC)
import Data.Version (showVersion)       

import Git
import Git.Libgit2 (LgRepo, HasLgRepo, OidPtr, lgFactory)

import Graphics.Vty.Attributes (black, blue, defAttr, green, red,
                                reverseVideo, yellow)
import Graphics.Vty.Input.Events (Event(..), Key(..))

import System.Environment (getArgs, getProgName)
import System.Exit (die, exitSuccess)

import Prelude hiding (head, log)


data ResourceName = Commits | LogView
  deriving (Eq, Ord, Show)


data CommitList =
  CommitList { _ncommits :: Int
             , _commits :: WL.List ResourceName CommitInfo
             , _current :: Maybe CommitInfo
             , _branch :: Maybe T.Text
             }
  

commitApp :: B.App CommitList e ResourceName
commitApp =
  let attrs = B.attrMap defAttr attrList
      attrList = [ (B.attrName "commit", B.fg yellow),
                   (B.attrName "summary", B.fg green),
                   (B.attrName "wip", black `B.on` green),
                   (B.attrName "selected", B.style reverseVideo),
                   (B.attrName "ref", B.fg red),
                   (B.attrName "header", black `B.on` blue)
                 ]

  in B.App
     { B.appDraw = drawApp
     , B.appChooseCursor = B.neverShowCursor
     , B.appHandleEvent = handleEvent
     , B.appStartEvent = pure ()
     , B.appAttrMap = const attrs
     }

     

withAttrName :: String -> B.Widget n -> B.Widget n
withAttrName n = B.withAttr (B.attrName n)


drawApp :: CommitList -> [B.Widget ResourceName]
drawApp cl =
  case _current cl of
    Just commit -> drawCommit commit
    _ -> drawList (_ncommits cl) (_branch cl) (_commits cl)


getSize :: Int -> Int
getSize size = ceiling (logBase 10 (fromIntegral size :: Double))


getLogName :: T.Text -> String
getLogName log = if T.count (T.pack "WIP") log == 0 then "summary" else "wip"


drawList ::
  Int
  -> Maybe T.Text
  -> WL.List ResourceName CommitInfo
  -> [B.Widget ResourceName]
drawList size mbranch list = [vBox [header, listBox]]
  where
    nchar = getSize size

    addBranch = case mbranch of
                 Just b -> (B.padRight (B.Pad 1) (B.txt (b <> ":")) <+>)
                 _ -> id

    header = withAttrName "header"
             . hCenter
             . addBranch
             $ B.str (show size <> " commit" <> if size > 1 then "s" else "")
             
    listBox = WL.renderList draw True list
    
    draw selected ci =
      let hash = T.take 6 (ciHash ci)
          commit = withAttrName "commit"
                   . B.padRight (B.Pad 1)
                   $ B.txt hash

          -- I want to combine the "ref" and "wip" markers but as written
          -- this does not work, and "ref" will 'win'.
          --
          withLog = withAttrName $ getLogName (ciTitle ci)
          withRef = if null (ciRefs ci) then id else withAttrName "ref"
          log = withLog
                . withRef
                . B.txt
                $ ciTitle ci

          wrap = if selected
                 then withAttrName "selected"
                 else id

          pos = B.padRight (B.Pad 1)
                . wrap
                . B.hLimit nchar
                . B.padRight B.Max
                . B.str
                $ show (ciPos ci)

      in hBox [pos, commit, log]


drawCommit :: CommitInfo -> [B.Widget ResourceName]
drawCommit ci = 
  let hash = ciHash ci
      commitBox = hBox [B.padRight (B.Pad 1) ((B.str . show . ciPos) ci),
                        withAttrName "commit" (B.txt hash)]

      refs = ciRefs ci
      refsBox = hBox (map (withAttrName "ref"
                           . B.padLeft (B.Pad 1)
                           . B.txt) refs)

      commit = if null refs
               then commitBox
               else vBox [commitBox, refsBox]
                
      author = B.txt ("Author: " <> ciAuthor ci <> " <" <> ciEmail ci <> ">")
      date = B.txt ("When:   " <> ciWhen ci)

      log = B.padTop (B.Pad 1)
            . withAttrName "summary"
            . B.withVScrollBars B.OnRight
            . B.viewport LogView B.Vertical
            $ B.txt (ciLog ci)

      {-
      files = withAttrName "files"
              $ B.txt (T.intercalate "\n" (ciFiles ci))
      -}
        
  in [vBox [commit, author, date, log {- , files -}]]


reverseList :: B.EventM ResourceName CommitList ()
reverseList = do
  st <- B.get
  B.put $ st { _commits = WL.listReverse (_commits st) }


getSel :: WL.List n e -> Maybe e
getSel list = snd <$> WL.listSelectedElement list


-- Options are:
--    q/Q   - exit
--    r/R   - reverse the list order
--    ->    - if _current is Nothing display the selected commit
--            otherwise nothing
--    <-    - if current is Just c then display the commit list
--            otherwise nothing
--    up/down - if current is Nothing then scroll the commit list
--              else possibly scroll the commit
--    page up/down and current is not Nothing - next or previous commit
--
handleEvent ::
  B.BrickEvent ResourceName e
  -> B.EventM ResourceName CommitList ()
handleEvent (B.VtyEvent (EvKey (KChar 'q') _)) = B.halt
handleEvent (B.VtyEvent (EvKey (KChar 'Q') _)) = B.halt

handleEvent (B.VtyEvent (EvKey (KChar 'r') _)) = reverseList
handleEvent (B.VtyEvent (EvKey (KChar 'R') _)) = reverseList

handleEvent (B.VtyEvent (EvKey KRight _)) = do
  st <- B.get
  let msel = getSel (_commits st)
  case _current st of
    Nothing -> B.put $ st { _current = msel }
    _ -> pure ()
    

handleEvent (B.VtyEvent (EvKey KLeft _)) = do
  st <- B.get
  case _current st of
    Nothing -> pure ()
    _ -> B.put $ st { _current = Nothing }


-- Pass the event handler to the
--    - list
--    - scrollport (if a commit is visible)
--
handleEvent ev = do
  st <- B.get
  let list = _commits st
      handle nlist = st { _commits = nlist }
      vp = B.viewportScroll LogView

      move f = st { _commits = slist,
                    _current = sel }
        where
          slist = f list
          sel = getSel slist

      scroll k =
        case k of
          KUp -> B.vScrollBy vp (-1)
          KDown -> B.vScrollBy vp 1
          KHome -> B.vScrollToBeginning vp
          KEnd -> B.vScrollToEnd vp
          _ -> pure ()

      reset = B.vScrollToBeginning vp

      -- ensure the scroll is reset when we switch commits
      put f = B.put (move f) >> reset
      
  case _current st of
    Nothing -> scrollHandler handle list ev
    _ -> case ev of
           B.VtyEvent (EvKey KPageUp _) -> put WL.listMoveUp
           B.VtyEvent (EvKey KPageDown _) -> put WL.listMoveDown
           B.VtyEvent (EvKey k _) -> scroll k
           _ -> pure ()


scrollHandler ::
  Ord n
  => (WL.List n e -> s)
  -> WL.List n e
  -> B.BrickEvent m f
  -> B.EventM n s ()
scrollHandler restore list (B.VtyEvent ev) = do
  nlist <- B.nestEventM' list (WL.handleListEvent ev)
  B.put (restore nlist)

scrollHandler _ _ _ = pure ()  -- assume we do nothing


-- | Given the prefix of a commit, return the commit data.
--
findACommit ::
  (HasLgRepo m, r ~ LgRepo, MonadGit r m)
  => OidPtr
  -> m (Maybe (Commit r))
findACommit oid = do
  obj <- lookupObject oid
  pure $ case obj of
           CommitObj c -> Just c
           _ -> Nothing


-- | Find all the commits from start to end.
--
findAllCommits ::
  (HasLgRepo m, r ~ LgRepo, MonadGit r m)
  => UTCTime
  -> M.Map (Oid r) [RefName]
  -> Commit r
  -> OidPtr
  -> m (WL.List ResourceName CommitInfo)
findAllCommits cTime refMap startC end = do
  commitoids <- listCommits (Just (commitOid startC)) (Tagged end)
  mcommits <- mapM (findACommit . unTagged) commitoids
  endC <- findACommit end

  -- It is not clear to me what listCommits returns; I thought I
  -- did not need to end endC in (and in some cases I don't, but
  -- I am sure it is needed in other cases. However, the check
  -- below does not work correctly in all cases.
  --
  let commits1 = startC : catMaybes (mcommits <> [endC])

      commits = case reverse commits1 of
        (c1:c2:rest) | commitOid c1 == commitOid c2 -> reverse (c2:rest)
        _ -> commits1
  
  -- trees <- mapM findATree (map commitTree commits)
  
  let -- acommits = zipWith3 comb [1..] commits trees
      acommits = zipWith comb [1..] commits
      comb = curry (grab cTime refMap)
      
  pure $ WL.list Commits (V.fromList acommits) 1


-- | Find the files in this tree.
--
{-
findATree ::
  (HasLgRepo m, r ~ LgRepo, MonadGit r m)
  => TreeOid r
  -> m [TreeFilePath]
findATree toid = do
  obj <- lookupObject (unTagged toid)
  case obj of
    TreeObj t -> map fst <$> listTreeEntries t 
    _ -> pure []

-}


-- Dang it: how do I find out what files were changed, not just what
-- files are in the repository at each commit.
--
data CommitInfo =
  CommitInfo { ciHash :: T.Text
             , ciRefs :: [T.Text]
             , ciTitle :: T.Text
             , ciLog :: T.Text
             , ciAuthor :: T.Text
             , ciEmail :: T.Text
             , ciWhen :: T.Text
             -- , ciFiles :: [T.Text]
             , ciPos :: Int
             }

-- | Convert a Commit into a simpler structure
--
-- For now we don't actually use the file info as I can't work
-- out how to find just the files that were changed in the commit.
--
grab ::
  (IsOid (Oid r), Ord (Oid r))
  => UTCTime  -- the current time, so we can create relative times
  -> M.Map (Oid r) [RefName]
  -- -> (Int, Commit r, [TreeFilePath])
  -> (Int, Commit r)
  -> CommitInfo
grab now refMap (pos, c {- , fs -}) =
  let log = if commitLog c == "" then "<no log>" else commitLog c
      title = case T.lines log of
                   x:_ -> x
                   _ -> "<no log>"  -- not really needed

      auth = commitAuthor c

      whenT = zonedTimeToUTC (signatureWhen auth)
      (ndays, tod) = timeToDaysAndTimeOfDay (diffUTCTime now whenT)
      delta_day = conv "day" ndays
      delta_hour = conv "hour" (todHour tod)
      delta_min = conv "minute" (todMin tod)

      delta_t = filter (not . null) [delta_day, delta_hour, delta_min]
      delta = intercalate ", " delta_t
      
      conv lbl v = case v of
                     0 -> ""
                     1 -> "one " <> lbl
                     _ -> show v <> " " <> lbl <> "s"

      -- Do we have any names to map to this tag?
      --
      oid = unTagged (commitOid c)
      hashVal = renderOid oid

      -- simplest sort is just length; could try to be clever about
      -- breaking up by the remote name, but not worth it
      --
      mRefs = M.lookup oid refMap
      refs = sortOn T.length (fromMaybe [] mRefs)
      
  in CommitInfo { ciHash = hashVal
                , ciRefs = refs
                , ciTitle = title
                , ciLog = log
                , ciAuthor = signatureName auth
                , ciEmail = signatureEmail auth
                , ciWhen = T.pack delta
                -- , ciFiles = map (T.pack . B8.unpack) fs
                , ciPos = pos
                }

  
initCommitList ::
  (HasLgRepo m, r ~ LgRepo, MonadGit r m)
  => UTCTime
  -> String       -- starting commit
  -> m (Maybe CommitList)
initCommitList cTime hash = do

  -- what are the references?
  --    remove prefixes
  --      refs/remotes/
  --      refs/heads/
  --
  refNames <- listReferences
  refIds <- mapM resolveReference refNames
  let refList =
        mapMaybe (\(a, mb) -> case mb of
                                Just b -> Just (b, [clean a])
                                _ -> Nothing)
        $ zip refNames refIds

      refMap = M.fromListWith (++) refList

      clean ref
        | T.isPrefixOf "refs/remotes/" ref = T.drop 13 ref
        | T.isPrefixOf "refs/heads/" ref = T.drop 11 ref
        | otherwise = ref
        
  -- ideally would use existsObject but it's not clear that actually
  -- works, so this code can fail. Also parseOid can fail too.
  --
  oid <- parseOid (T.pack hash)
  mCommit <- findACommit oid
  mRef <- resolveReference "HEAD"
  
  let comb = do
        a <- mCommit
        b <- mRef
        pure (a, b)

  case comb of
    Just (start, end) -> do
      commits <- findAllCommits cTime refMap start end

      -- Guess the branch name.
      --
      let branch = case M.lookup end refMap of
                     Just (refName:_) -> Just refName
                     _ -> Nothing

      -- Start with the latest commit first and, if there's only
      -- one commit, select that commit.
      --
      let scommits = WL.listMoveToBeginning $ WL.listReverse commits
          ncommits = length (WL.listElements scommits)
          scurrent = if ncommits == 1
                     then getSel scommits
                     else Nothing
                         
      pure $ Just $ CommitList {
        _ncommits = ncommits
        , _commits = scommits
        , _current = scurrent
        ,_branch = branch }

    _ -> pure Nothing


process :: String -> IO ()
process commit = do
  cTime <-getCurrentTime
  minit <- withRepository lgFactory "." $ initCommitList cTime commit
  case minit of
    Just initVal -> view initVal
    _ -> die $ "Unable to find commit " <> commit
      

view :: CommitList -> IO ()
view = void . B.defaultMain commitApp
  

reportVersion :: IO ()
reportVersion = do
  name <- getProgName
  putStrLn (name <> ": v" <> showVersion P.version <> " (" <>
           SI.compilerName <> " " <> showVersion SI.fullCompilerVersion <>
           " " <> SI.os <> " " <> SI.arch <> ")")
  exitSuccess


main :: IO ()
main = do
  args <- getArgs
  when ("--version" `elem` args) reportVersion
  case args of
    [commit] -> process commit
    _ -> do
      name <- getProgName
      die $ "Usage: " <> name <> " commit"
      

