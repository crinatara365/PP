-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

-- ==================================================

module Tasks where

import Dataset
import Data.List
import Data.Function
import Data.Maybe
import Text.Printf
import Data.Array

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]
type ColumnName = String

-- Prerequisities
split_by :: Char -> String -> [String]
split_by x = foldr op [""]
  where op char acc
            | char == x = "":acc
            | otherwise = (char:head(acc)):tail(acc)

read_csv :: CSV -> Table
read_csv = (map (split_by ',')) . (split_by '\n')

write_csv :: Table -> CSV
write_csv = (foldr (++) []).
            (intersperse "\n").
            (map (foldr (++) [])).
            (map (intersperse ","))


{-
    TASK SET 1
-}


-- Task 1
media_aritmetica :: [Value] -> Float
media_aritmetica l = (sum (map read l)) / (genericLength l)

compute_aux :: Table -> Table
compute_aux [] = []
compute_aux m = [head (head m ) : [(printf "%.2f")(media_aritmetica (tail (head m )))]] ++ (compute_aux (tail m))

compute_average_steps :: Table -> Table
compute_average_steps m = ("Name":("Average Number of Steps":[])) : (compute_aux (tail m))

-- Task 2

-- Number of people who have achieved their goal:
suma :: [Value] -> Int
suma [] = 0
suma l = (read (head l) :: Int) + suma (tail l)

get_passed_people_num_aux :: Table -> Int
get_passed_people_num_aux [] = 0
get_passed_people_num_aux m  
                            | suma (tail (head m)) >= 1000 = 1 + get_passed_people_num_aux(tail m)
                            | otherwise = 0 + get_passed_people_num_aux(tail m)

get_passed_people_num :: Table -> Int
get_passed_people_num m = get_passed_people_num_aux (tail m)

-- Percentage of people who have achieved their:
total_people :: Table -> Int
total_people [] = 0
total_people m = 1 + total_people (tail m)

get_passed_people_percentage :: Table -> Float
get_passed_people_percentage m = (fromIntegral (get_passed_people_num m) :: Float) / (fromIntegral (total_people m) :: Float)

-- Average number of daily steps
sum_per_person :: [Value] -> Float
sum_per_person l = sum (map read l)

table_of_sum :: Table -> Table
table_of_sum [] = []
table_of_sum m = [head (head m ) : [(printf "%.2f")(sum_per_person (tail (head m )))]] ++ (table_of_sum (tail m))

total_sum :: Table -> Float
total_sum [] = 0
total_sum m = (read(head(tail(head m))) :: Float) + total_sum(tail m) 

get_steps_avg :: Table -> Float
get_steps_avg m = (total_sum(table_of_sum(tail m))) / ((genericLength m)-1)

-- Task 3
remove_first :: Table -> Table
remove_first [] = []
remove_first(r:t) = (tail r) : (remove_first t)

sum_per_h :: Table -> Float
sum_per_h [] = 0
sum_per_h (r:t) = ((read(head(tail r)) :: Float) + sum_per_h t) 

average_per_h :: Table -> [Value]
average_per_h ([_]:_) = []
average_per_h m = ((printf "%.2f")((sum_per_h m) / (genericLength m))) : (average_per_h(remove_first m))

get_avg_steps_per_h :: Table -> Table
get_avg_steps_per_h m = ["H10","H11","H12","H13","H14","H15","H16","H17"]:(average_per_h(tail m)):[]

-- Task 4
get_last_3_columns :: Table -> Table
get_last_3_columns m = remove_first(remove_first(remove_first m))

check_range1 :: Table -> Integer
check_range1 [] = 0
check_range1 (r:t) 
                    | (read(head r) :: Float) < 50 = 1 + check_range1 t
                    | otherwise = 0 + check_range1 t    

check_range2 :: Table -> Integer
check_range2 [] = 0
check_range2 (r:t) 
                    | (read(head r) :: Float) >= 50 && (read(head r) :: Float) < 100 = 1 + check_range2 t
                    | otherwise = 0 + check_range2 t

check_range3 :: Table -> Integer
check_range3 [] = 0
check_range3 (r:t) 
                    | (read(head r) :: Float) >= 100 && (read(head r) :: Float) < 500 = 1 + check_range3 t
                    | otherwise = 0 + check_range3 t                                                        

build_VeryActive :: Table -> [Value]
build_VeryActive [] = []
build_VeryActive m = "VeryActiveMinutes"
                    : show((check_range1(get_last_3_columns m)))
                    : show((check_range2(get_last_3_columns m)))
                    : show((check_range3(get_last_3_columns m)))
                    : []

build_FairlyActive :: Table -> [Value]
build_FairlyActive [] = []
build_FairlyActive m = "FairlyActiveMinutes"
                    : show((check_range1(get_last_3_columns(remove_first m))))
                    : show((check_range2(get_last_3_columns(remove_first m))))
                    : show((check_range3(get_last_3_columns(remove_first m))))
                    : []    

build_LightlyActive :: Table -> [Value]
build_LightlyActive [] = []
build_LightlyActive m = "LightlyActiveMinutes"
                    : show((check_range1(get_last_3_columns(remove_first(remove_first m)))))
                    : show((check_range2(get_last_3_columns(remove_first(remove_first m)))))
                    : show((check_range3(get_last_3_columns(remove_first(remove_first m)))))
                    : []                                    

get_activ_summary :: Table -> Table
get_activ_summary m = ["column","range1","range2","range3"] 
                  : (build_VeryActive(tail m))
                  : (build_FairlyActive(tail m))
                  : (build_LightlyActive(tail m))
                  : []

-- Task 5
get_first_2 :: Table -> Table
get_first_2 [] = []
get_first_2 (r:t) = ((head r):(head(tail r)):[]):(get_first_2 t)

sorted_list :: Table -> [Integer]
sorted_list [] = []
sorted_list (r:t) = sort(((read(head(tail r))) :: Integer) : (sorted_list t))

search_people :: Table -> Integer -> [Value]
search_people [] x = []
search_people m x 
                  | (read(head(tail(head m))) :: Integer) == x = (head(head m)):search_people (tail m) x
                  | otherwise = search_people (tail m) x

check_if_search :: Table -> [Integer] -> [Value]
check_if_search m (x:[]) = sort(search_people m x)
check_if_search m l = if (head l) /= (head(tail l)) then sort(search_people m (head l)) else []

add_names :: [Integer] -> [Integer] -> [Value] -> Table
add_names l l_final [] = []
add_names [] l_final v = []
add_names l l_final v 
              | (head l_final) == (head l) = build_table v l_final
              | otherwise = add_names l (tail l_final) v  

build_table :: [Value] -> [Integer] -> Table
build_table v [] = []
build_table [] l = []
build_table v l = ((head v):(show(head l)):[]):(build_table (tail v) (tail l))                

aux :: [Integer] -> [Integer] -> Table -> Table
aux [] _ _ = []
aux _ [] _ = []
aux _ _ [] = []
aux l l_final m 
        | (check_if_search m l) == [] = aux (tail l) l_final m
        | otherwise = (add_names l l_final (check_if_search m l)) ++ (aux (tail l) l_final m)

get_ranking :: Table -> Table
get_ranking [] = []
get_ranking m = ["Name","Total Steps"] : (aux (sorted_list (tail m)) (sorted_list (tail m)) (tail m))

-- Task 6
get_first_4 :: [Value] -> [Value]
get_first_4 l = (head l):(head(tail l)):(head(tail(tail l))):(head(tail(tail(tail l)))):[]

get_last_4 :: [Value] -> [Value]
get_last_4 l = tail(tail(tail (tail l))) 

check_if_pozitive :: Float -> Value
check_if_pozitive x 
                    | x < 0 = (printf "%.2f")((-1)*x)
                    | otherwise = (printf "%.2f")x   

new_tabel :: Table -> Table
new_tabel [] = []
new_tabel (r:t) = [(head r) : [check_if_pozitive((media_aritmetica (get_first_4(tail r)))-(media_aritmetica (get_last_4(tail r))))]] 
              ++ (new_tabel t)              

sort_by_difference :: Table -> [Float]
sort_by_difference [] = []
sort_by_difference (r:t) = sort((read(head(tail r)) :: Float) : (sort_by_difference t))

search_people2 :: Table -> Float -> [Value]
search_people2 [] x = []
search_people2 m x 
                  | (read(head(tail(head m))) :: Float) == x = (head(head m)):search_people2 (tail m) x
                  | otherwise = search_people2 (tail m) x

check_if_search2 :: Table -> [Float] -> [Value]
check_if_search2 m (x:[]) = sort(search_people2 m x)
check_if_search2 m l = if (head l) /= (head(tail l)) then sort(search_people2 m (head l)) else []

add_names2 :: [Float] -> [Float] -> [Value] -> Table
add_names2 l l_final [] = []
add_names2 [] l_final v = []
add_names2 l l_final v 
              | (head l_final) == (head l) = build_table2 v l_final
              | otherwise = add_names2 l (tail l_final) v  

build_table2 :: [Value] -> [Float] -> Table
build_table2 v [] = []
build_table2 [] l = []
build_table2 v l = [(head v):[(printf "%.2f")(head l)]]++(build_table2 (tail v) (tail l))                

aux2 :: [Float] -> [Float] -> Table -> Table
aux2 [] _ _ = []
aux2 _ [] _ = []
aux2 _ _ [] = []
aux2 l l_final m 
        | (check_if_search2 m l) == [] = aux2 (tail l) l_final m 
        | otherwise = (add_names2 l l_final (check_if_search2 m l)) ++ (aux2 (tail l) l_final m)

sorted_table :: Table -> Table    
sorted_table m = aux2 (sort_by_difference(new_tabel m)) (sort_by_difference(new_tabel m)) (new_tabel m)    

add_columns :: Table -> Table -> Table -> Table
add_columns x [] y = []
add_columns (r1:t1) (r2:t2) m
                              | (head r1) == (head r2) = [(head r2):[(printf "%.2f")(media_aritmetica (get_first_4(tail r1)))]
                                                        ++ [(printf "%.2f")(media_aritmetica (get_last_4(tail r1)))] 
                                                        ++ (tail r2) ] 
                                                        ++ add_columns m t2 m
                              | otherwise = add_columns t1 (r2:t2) m                          

get_steps_diff_table :: Table -> Table
get_steps_diff_table (r:t) = ["Name","Average first 4h","Average last 4h","Difference"] : (add_columns t (sorted_table t) t)
 
-- Task 7

-- Applies the given function to all the values
vmap :: (Value -> Value) -> Table -> Table
vmap f [] = []
vmap f m = (map f (head m)) : (vmap f (tail m))

-- Task 8

-- Applies the given function to all the entries
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f s [] = []
rmap f s m = s : ((f (head m)) : (rmap f s (tail m)))

get_sleep_total :: Row -> Row
get_sleep_total r = (head r) : [(printf "%.2f")(sum_of_mins(tail r))]
    where sum_of_mins [] = 0
          sum_of_mins r = (read(head r) :: Float) + sum_of_mins (tail r)


{-
    TASK SET 2
-}

-- Task 1
sort_by_name :: Int -> Table -> Table
sort_by_name index [x] = [x]
sort_by_name index (r:t)  
                | r !! index == (head t) !! index = r : (sort_by_name index t) 
                | otherwise = [r] 

get_list :: Int -> Table -> [Integer]
get_list index [] = []
get_list index table = (read((head table) !! index) :: Integer) : (get_list index (tail table))

get_table :: [Integer] -> Table -> Table -> Int -> Table
get_table [] _ _ _ = []
get_table (h:l) (r:t) table index 
                | show h == r !! index = r : (get_table l (delete r table) (delete r table) index) 
                | otherwise = get_table (h:l) t table index

g :: ColumnName -> Table -> Table
g column table = get_table (sortBy compare (get_list (fromJust(elemIndex column (head table))) (tail table))) 
                                 (tail table) 
                                 (tail table) 
                                 (fromJust(elemIndex column (head table)))

h :: Table -> Int -> Table
h [] index = []
h t1 index = (sortBy compare (sort_by_name index t1)) ++ (h (drop (genericLength (sort_by_name index t1)) t1) index)

tsort :: ColumnName -> Table -> Table
tsort column table = (head table) : (h (g column table) (fromJust(elemIndex column (head table)))) 

-- Task 2
vunion :: Table -> Table -> Table
vunion t1 t2 = if(head t1 == head t2) then t1++(tail t2) else t1

-- Task 3
null_list :: Integer -> Row
null_list length 
        | length > 0 = "": null_list (length-1)
        | otherwise = []

pad :: Table -> Table -> Integer -> Table
pad [] [] length = []
pad t1 [] length = ((head t1)++(null_list (length - genericLength (head t1)))) : (pad (tail t1) [] length )
pad t1 t2 length = ((head t1)++(head t2)) : (pad (tail t1) (tail t2) length)

hunion :: Table -> Table -> Table
hunion [] t2 = []
hunion t1 [] = []
hunion t1 t2 
        | genericLength t1 < genericLength t2 = pad t2 t1 (genericLength (head t1) + genericLength (head t2))
        | genericLength t1 > genericLength t2 = pad t1 t2 (genericLength (head t1) + genericLength (head t2))
        | otherwise = pad t1 t2 (genericLength (head t1) + genericLength (head t2))

-- Task 4
exists :: String -> Table -> Int -> Bool
exists key [] index = False
exists key t index
        | key == ((head t) !! index) = True
        | otherwise = exists key (tail t) index

get_index :: Row -> Table -> Table -> Int
get_index row (r:t) initial 
        | (head row) == (head r) = (fromJust(elemIndex r initial))
        | otherwise = get_index row t initial

add_by_key :: [Int] -> Table -> Table -> Table 
add_by_key index [] _ = []
add_by_key index (r1:t1) t2
        | exists (r1 !! (index !! 0)) t2 (index !! 1) == True = (r1 ++ (tail(t2!!(get_index r1 t2 t2)))) : (add_by_key index t1 t2) 
        | otherwise = add_by_key index t1 t2

find_key :: String -> Row -> Row -> [Int]
find_key key r1 r2 = [(fromJust(elemIndex key r1)), (fromJust(elemIndex key r2))]

tjoin :: ColumnName -> Table -> Table -> Table            
tjoin key_column t1 t2 = add_by_key (find_key key_column (head t1) (head t2)) t1 t2

-- Task 5
y :: (Row -> Row -> Row) -> Row -> Table -> Table
y f row [] = []
y f row tabel = (f row (head tabel)) : (y f row (tail tabel))

x :: (Row -> Row -> Row) -> Table -> Table -> Table
x f [] t2 = []
x f t1 t2 = (y f (head t1) t2) ++ (x f (tail t1) t2)

cartesian :: (Row -> Row -> Row) -> [ColumnName] -> Table -> Table -> Table
cartesian new_row_function new_column_names t1 t2 = new_column_names : (x new_row_function (tail t1) (tail t2))

-- Task 6
-- se parcurge un row si se adauga doar elementele care au indicii in vectorul indexes
b :: [Int] -> Row -> Row
b [] row = []
b indexes row = ((row !! (head indexes)) : (b (tail indexes) row))

-- se apeleaza functia b pe fiecare rand din tabelul dat
a :: [Int] -> Table -> Table
a indexes [] = []
a indexes table = (b indexes (head table)) : (a indexes (tail table)) 

-- se obtine vectorul de indici ai coloanelor ce trebuie selectate
get_indexes :: [String] -> Row -> Table -> [Int]
get_indexes [] _ _ = []
get_indexes columns [] table = get_indexes (tail columns) (head table) table
get_indexes columns row table
                | (head columns) == (head row) = ((fromJust(elemIndex (head row) (head table)))) : (get_indexes (tail columns) (head table) table)
                | otherwise = get_indexes columns (tail row) table        

projection :: [ColumnName] -> Table -> Table
projection columns_to_extract t = a (get_indexes columns_to_extract (head t) t) t                         

-- Task 7
through_rows :: Int -> Table -> (Value -> Bool) -> Table
through_rows index [] condition = []        
through_rows index table condition
                | condition ((head table) !! index) == True = (head table) : (through_rows index (tail table) condition)
                | otherwise = through_rows index (tail table) condition

filterTable :: (Value -> Bool) -> ColumnName -> Table -> Table
filterTable condition key_column t = (head t) : (through_rows (fromJust(elemIndex key_column (head t))) (tail t) condition)