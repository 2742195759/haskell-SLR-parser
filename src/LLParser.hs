module LLParser (Symbol(..), Produce(..), parser, getSLRparser) where 

import Table (showTable, default_option, test_table, makeTable)
import qualified Data.HashMap as HM
import qualified Data.Hashable as Hash
import Data.List (nub, groupBy, elemIndex)
import Debug.Trace (trace)
import Control.Monad (liftM)

-- TODO: 
-- 0. parser function definition : [done]
-- 1. add production eval function 
-- 2. follow set / head set of each non-terminal symbol
-- 3. epsilon remove algorithm
-- 4. SLR parser construct

data Symbol = Symbol {
    name :: String
} deriving (Eq, Ord)

is_terminal :: Symbol -> Bool
is_terminal (Symbol name) = (not . and) [head name == '<', last name == '>']

instance Show Symbol where
    show symbol = "# " ++ name symbol ++ " #"

instance Hash.Hashable Symbol where
    hashWithSalt salt symbol = Hash.hashWithSalt salt (name symbol) + Hash.hashWithSalt salt (is_terminal symbol)

data Produce = Produce {
    left :: Symbol, 
    right :: [Symbol]
    -- eval_fn :: ???
} deriving (Eq, Ord)

instance Show Produce where
    show produce = (name $ left produce) ++ "->" ++ (foldl (\acc x -> acc ++ (name x)) "" $ right produce)

instance Hash.Hashable Produce where
    hashWithSalt salt produce = Hash.hashWithSalt salt (left produce) + Hash.hashWithSalt salt (right produce)

data SLRTable = SLRTable {
    action :: HM.Map (State, Symbol) Action,
    goto :: HM.Map (State, Symbol) State, 
    state :: [ItemSet]
} deriving (Eq)

hasConflict :: (State, Symbol) -> SLRTable -> Bool
hasConflict (s, sym) table = if HM.member (s, sym) (action table) || HM.member (s, sym) (goto table) then True else False

insertAction :: (State, Symbol) -> Action -> SLRTable -> SLRTable
insertAction (s, sym) act table = if trace ("insertAction: " ++ show (s, sym)) hasConflict (s, sym) table then error $ "reduce-shift / reduce-reduce conflict: " ++ show s else addState s $ SLRTable (HM.insert (s, sym) act (action table)) (goto table) (state table)

insertGoto :: (State, Symbol) -> State -> SLRTable -> SLRTable
insertGoto (s, sym) next table = addState s $ SLRTable (action table) (HM.insert (s, sym) next (goto table)) (state table)

addState :: ItemSet -> SLRTable -> SLRTable
addState s table = SLRTable (action table) (goto table) (state table ++ [s])

instance Show SLRTable where
    show table = "\nstate: " ++ show_states ++ "\n action: \n" ++ show_table
        where states = (nub . state) table
              all_symbols = let s1 = map (\(s, sym)->sym) $ HM.keys (action table)
                                s2 = map (\(s, sym)->sym) $ HM.keys (goto table)
                            in nub $ s1 ++ s2
              (show_states, _) = foldl (\(acc, num) x -> (acc ++ "\n" ++ show num ++ ": " ++ show x, num+1)) ("", 0) states
              show_table = showTable default_option [0..(length states - 1)] (map show all_symbols) action_table
              action_table = (liftM.liftM) (\(s, sym) -> (if (is_terminal sym) then cell_str_term else cell_str_nonterm) s sym) $ card_prod
              card_prod = makeTable [0..(length states - 1)] all_symbols
              states_idx s = case elemIndex s states of
                    Just idx -> idx
                    Nothing -> error "state not found"
              cell_str_term s sym = case HM.lookup (states !! s, sym) (action table) of
                        Just (Shift next) -> "s " ++ show (states_idx next)
                        Just (Reduce p) -> "r " ++ show (left p)
                        Just Accept -> "acc"
                        Just Error -> "   "
                        Nothing -> "   "
              cell_str_nonterm s sym = case HM.lookup (states !! s, sym) (goto table) of
                        Just s -> "goto:  " ++ show (states_idx s)
                        Nothing -> "   "

type State = ItemSet
data Action = Shift State | Reduce Produce | Accept | Error deriving (Eq, Show)
data Item = Item {
    produce :: Produce,
    dot :: Int
} deriving (Eq, Ord)
instance Show Item where
    show Item {produce=p, dot=d} = take dot_start origin_str ++ " || " ++ (drop dot_start origin_str)
         where truncated = show $ Produce (left p) (take d $ right p)
               dot_start = length truncated
               origin_str = show p

instance Hash.Hashable Item where
    hashWithSalt salt item = Hash.hashWithSalt salt (produce item) + Hash.hashWithSalt salt (dot item)

isReducable :: Item -> Bool
isReducable item = dot (item) == (length $ right $ produce item)

getExpectedSymbol :: Item -> (Symbol, Item)
getExpectedSymbol item = ((right $ produce item) !! (dot item), Item (produce item) ((dot item) + 1))

type ItemSet = [Item]

--- closure operation
closure :: [Produce] -> ItemSet -> ItemSet
closure produces items = unary_closure_do items (closure_operator produces)

unary_closure_do :: Eq a => [a] -> (a -> [a]) -> [a]
unary_closure_do xs op = if (length onepass == length xs) then xs else unary_closure_do onepass op
    where onepass = nub $ xs ++ (concat $ map op xs)

closure_operator :: [Produce] -> Item -> ItemSet
closure_operator ps item = case (length $ right $ produce item) > (dot item) of 
                    False -> []
                    True  -> let symbol = [(right $ produce item) !! (dot item)]
                            in  map (\p -> Item p 0)  $ filter (\x -> (left x) == (head symbol)) ps

getSLRparser :: [Produce] -> SLRTable
getSLRparser produces = trace ("debug: " ++ show root) construct_table produces [] [root] $ SLRTable HM.empty HM.empty []
    where root = closure produces [Item (head produces) 0]

construct_table :: [Produce] -> [ItemSet] -> [ItemSet] -> SLRTable -> SLRTable
construct_table produces visited [] table = table
construct_table produces visited (x:xs) table = if trace ("construct_table for: " ++ show x ++ show nexts) not has_visited then construct_table produces (x:visited) (nub new_queue) final_table else construct_table produces visited xs table 
    where new_queue = xs ++ map (\(_, x)->closure produces x) nexts 
          has_visited = any (\m -> m == x) visited
          processed_table = foldl (\acc operand -> insertShiftItem produces x operand acc) table nexts
          nexts = nextItemSetMeetingSymbol produces x 
          final_table = insertReduceItem produces x processed_table 

insertShiftItem :: [Produce] -> ItemSet -> (Symbol, ItemSet) -> SLRTable -> SLRTable
insertShiftItem produces start (symbol, next) table = case is_terminal symbol of
            True ->  insertAction (start, symbol) (Shift next_itemset) table
            False -> insertGoto  (start, symbol) next_itemset table
    where next_itemset = closure produces next

allSymbols :: [Produce] -> [Symbol]
allSymbols produces = let tmp = nub $ concat $ map (\p -> (left p):(right p)) produces
                      in  nub $ tmp ++ [Symbol "eof", Symbol "sof"]

insertReduceItem :: [Produce] -> ItemSet -> SLRTable -> SLRTable
insertReduceItem produces current table = case length reduce_items of
        0 -> table
        _ -> case all_equal of 
            True ->  let all_syms = filter is_terminal $ allSymbols produces
                     in  foldl (\t s->insertAction (current, s) (Reduce $ produce head_item) t) table all_syms
            False -> error "reduce-reduce conflict."
    where reduce_items = filter isReducable current
          head_item = head reduce_items
          all_equal = all (\x -> x == head_item) reduce_items

nextItemSetMeetingSymbol :: [Produce] -> ItemSet -> [(Symbol, ItemSet)]
nextItemSetMeetingSymbol produces set = trace ("non_reducable_set=" ++ show non_reducable_set) res
    where non_reducable_set = filter (not . isReducable) set
          expected_set = map getExpectedSymbol non_reducable_set
          grouped_set = groupBy (\x y -> fst x == fst y) expected_set :: [[(Symbol, Item)]]
          res = map (\xs -> ((fst . head) xs, closure produces $ map snd xs)) grouped_set :: [(Symbol, ItemSet)]
          {-after_remove = filter (\x -> (snd x) /= set) before_remove-}

parser :: (String -> [Symbol]) -> String -> SLRTable -> Bool
parser tokenizer stream table = trace ("tokens:" ++ show tokenized_stream) parser_do tokenized_stream table [head $ state table]
    where tokenized_stream = [Symbol "sof"] ++ tokenizer stream ++ [Symbol "eof", Symbol "eof"]

parser_do :: [Symbol] -> SLRTable -> [State] -> Bool
parser_do (x:xs) table (s:ss) = case trace ("\nparser_do: " ++ show (x, s) ++ "\nstack: " ++ show (s:ss)) HM.lookup (s, x) (action table) of
        Just (Shift next) -> trace ("Shift" ++ show s) parser_do xs table (next:s:ss)
        Just (Reduce (Produce (Symbol "<Root>") _)) -> trace ("Recv <Root> | Remain: " ++ (show $ xs)) length xs == 0
        Just (Reduce p) -> let pop_number = length $ right p
                               add_sym = left p
                               (new_s:new_ss) = drop pop_number (s:ss)
                           in case HM.lookup (new_s, add_sym) (goto table) of
                                Just next -> trace ("Reduce" ++ show s) parser_do (x:xs) table (next:new_s:new_ss)
                                Nothing -> trace ("Reduce error !") False
        Just Error -> trace ("Error") False
        Nothing -> trace ("Nothing") False
praser_do [] _ stack = trace ("end with" ++ show stack) stack
praser_do _ _ _ = False
