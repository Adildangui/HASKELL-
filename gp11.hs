
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.List
import Data.Array.IO					--imported for IO array operations (mutable arrays without changing it as a whole)
import Data.List 					--using functions like (!!) defined in
import System.IO  
import Control.Monad
import Control.Concurrent				--
import System.Posix.Process
import System.Process					--for running mergesort and quicksort concurrently
import Data.IORef					--imported for using global variables 
import qualified Data.ByteString as Str
import System.Environment (getArgs)
import qualified Control.Exception as Exc		--to deal with error return value of root function
import System.Posix.Unistd
import Prelude hiding (catch)
import Control.Exception				--above two imports are for error handling

data SplayTree s  = 					--create new data type splaytree which can either be a leaf of a splayree with three
  SplayTree s (SplayTree s) (SplayTree s)		--attributes (root , left subtree and right subtree)
  | Leaf



							--this function irerates index from (0..size(list)-1) and in case of match with key 
							--puts the index in an array and returns the array


find_index list key = [ (index+1,list !! index) | index <-[0..(length(list)-1)] , (key == (list!!index))]	



							--this function searches for the key in the splaytree if found then the key is splayed 								--to the top else the parent (nearest) key is splayed up 

seek :: (Ord s) => s-> SplayTree s-> SplayTree s 	
seek _ Leaf = Leaf					--base case in case hit the leaf return leaf		
seek search_node copy_splay@(SplayTree current_node left_sub right_sub) =

  if (search_node == current_node) then copy_splay	--in case the key is found then pass this key to above recurrsions to spaly up

  else if (search_node < current_node) then		--else if serach key is < current key --> move left to search for the key

    	case seek search_node left_sub of		-- tree like    \                   or        /
							--		 o <- copy_splay             o  <- copy splay
							--		/ <-left_tree	            / <- left tree
     	 Leaf -> copy_splay
     	 left_tree -> right_rotate left_tree copy_splay
       else 						--else if search key > current key --> move to right tree and continue search
   	 case seek search_node right_sub of
    	  Leaf -> copy_splay				--opposite cases of above
    	  right_tree -> left_rotate copy_splay right_tree



							--first arument is left child and second argument is parent
							--detaches right node of the child and makes it left child of parent 
							--parent becoes child of old_child

right_rotate :: (Ord s) => SplayTree s-> SplayTree s-> SplayTree s 
right_rotate (SplayTree key1 left_sub1 right_sub1) (SplayTree key2 left_sub2 right_sub2) =
			SplayTree key1 left_sub1 (SplayTree key2  right_sub1 right_sub2)

							--first arument is parent and second argument is right child
							--detaches left node of the child and makes it left child of parent 
							--parent becoes child of old_child

left_rotate :: (Ord s) => SplayTree s-> SplayTree s-> SplayTree s 
left_rotate (SplayTree key1 left_sub1 right_sub1) (SplayTree key2 left_sub2 right_sub2) =
  			SplayTree key2  (SplayTree key1 left_sub1 left_sub2) right_sub2




insert_splay :: (Ord s) => s -> SplayTree s -> SplayTree s -- function for insert
insert_splay insert_key  splay =
  case (seek insert_key splay) of			   --make a call to seek with insert key if the tree is empty make a tree with root=key
    Leaf -> (SplayTree insert_key Leaf Leaf)
    modifed_tree@(SplayTree current_root left right) 	   --otherwise modified tree now has the splay tree with nearest element to insert_key
        						   --splayed up
	
	| current_root < insert_key ->			   -- depending on current_root < insert_key take the action (trivial)		   
		 SplayTree insert_key  (SplayTree current_root left Leaf) right
        |otherwise ->
		 SplayTree insert_key  left (SplayTree current_root Leaf right)



root :: SplayTree s -> s				   --function returns root of the tree/subtree. error in case the tree/subtree is empty
root Leaf = error "root of empty tree"
root (SplayTree s left right) = s


rightchild :: (Ord s) => SplayTree s -> SplayTree s	   --returns the right child (useful to get recent history of search)
rightchild (SplayTree s left right) = right

leftchild :: (Ord s) => SplayTree s -> SplayTree s	   --returns the left child (useful to get recent history of search)
leftchild t@(SplayTree s left right) = left



list_to_splay :: (Ord s) => [s] -> SplayTree s 		   --inserts the elements into splay folding array from left	
list_to_splay [] = Leaf
list_to_splay l = foldl (\ a(s)-> insert_splay s a) Leaf l




f1 :: [String] -> [Int]					   --function to get int from string while reading from file
f1 = map read

f' :: [Int] -> [String]					   --function to write back integer in string format to file
f' = (>>= return.show)

f2 :: [String] -> [String]				   --function to get compatible string to list from string while reading from file
f2 = map read

getNumber::IO Int					   --function to to prompt user to input a number and make the string into Num type
getNumber = do
    num <- getLine
    return (read num)



merge :: (Ord a) => [a] -> [a] -> [a]		           --function to merge two lists for mergesort
merge list [] = list
merge [] list = list
merge (headleft:leftlist) (headright:rightlist) = if (headleft>headright) --make the least one the head of the list and recurrse
						  then headright:merge (headleft:leftlist) rightlist
    		      				  else headleft:merge leftlist (headright:rightlist) 


mergesort :: (Ord a) => [a] -> [a]			   --mergesort function call mergesort on two subarrays split at middle and 
mergesort []=[]
mergesort list = if  (length list) > 1 			   --and then call merge on both the lists ..recurse until list is empty
	       	 then merge (mergesort leftlist) (mergesort rightlist)
   	       	 else list
    			where (leftlist,rightlist) = (take mid list, drop mid list)
					where mid = (length list) `div` 2 

ms::(Ord a,Show a)=>([a],IORef Int)->IO()		   --function to check if mergesort returns first or quicksort
ms(a,globl)=do
	let b=mergesort(a)
	out<-readIORef globl
	if out/=1					   --in case quicksort returns late (globl set to 1)
	   then do 
	   	modifyIORef globl (+1)			   --otherwise mark mergesort returned first ans print the list
		print b
	   else
		putStrLn  ""			
	


quicksort :: (Ord a) => [a] -> [a]			    --function for quicksort
quicksort [] = []	
							    --split at the middle such that the elements 
							    --to the right are > mid and left are<mid

quicksort (first:rest) = quicksort smaller ++ mid ++ quicksort larger	
				where        smaller = [a | a<-rest, a<first]
 					     larger = [c | c<-rest, c>first]
					     mid =   [b | b<-rest, b==first] ++ [first]

qs::(Ord a,Show a)=>([a],IORef Int)-> IO()		     --function to check if mergesort returns first or quicksort
qs(a,globl)=do
	
	let b=quicksort(a)
	out<-readIORef globl
	if out/=1					     --in case quicksort returns late (globl set to 1)
	   then do 
	   	modifyIORef globl (+1)			     --otherwise mark quicksort returned first ans print the list
		print b
	   else
		putStrLn  ""			
	

	
add_element :: (Ord a) => a -> [a]  -> [a]		     --adds an element to the start of the list
add_element m [] = [m]					     --in case list empty add to list
add_element x xs = x:xs					     --else add to the start


							     --function to print line numbers in files where the string has substring entered 
							     --by users
containing_sub  list key = [ (i+1,list !! i) | i<-[0..(length(list)-1)] , (sub_string key (list!!i))]	



sub_string :: String -> String -> Bool			     -- checks for each string if the other one is substring
sub_string (xhead:xtail) [] = False			     -- return false if the suprestring is empty and substring is non empty
sub_string x_to_search in_y =
    if (match x_to_search in_y == True) then True	     --if entire string matches return true
    else if (sub_string x_to_search (tail in_y) == True) then True	--else search in the tail of superstring 
	else
    		False						--otherwise return false



match :: String -> String -> Bool			     --function to match individual character of the list
match [] in_y = True					     --substring empty superstring non empty in recurrsion return true
match (xhead:xtail) [] = False				     --return false if the suprestring is empty and substring is non empty
match (xhead:xtail) (yhead:ytail) = (xhead==yhead) && match xtail ytail -- if head of both match then match tail


						
head_match :: String -> String -> Bool			     --checks if a string starts with certain substing
head_match [] y = True					     --similar as above
head_match (x:xs) [] = False
head_match (x:xs) (y:ys) = (x == y) && head_match xs ys


							     --function to print line numbers in files where the string starts with substring 								     --entered by user
start_string  list key = [ (i+1,list !! i) | i<-[0..(length(list)-1)] , (head_match key (list!!i))]	


							     --function to print line numbers in files where the string has strlen 								     --entered by user
strlen_search  list key = [ (i+1,list !! i) | i<-[0..(length(list)-1)] , key==length(list!!i)]

	

delete_from_list :: Eq a => a -> [a] -> [a]		      --delete a query from history .return array without element to be deleted
delete_from_list deleted xs = [ xs!!i | i <- [0..(length (xs)-1)], (is_equal deleted (xs!!i))==False ]	

				
is_equal ::Eq a => a -> a -> Bool			      -- check if two numbers are equal for deletion
is_equal key1 key2 = 
	if key1==key2 then True
		else False


forceList::[a]->()					      --force closing of file
forceList [] = ()
forceList (head:tail) = forceList tail



main = do


	putStrLn "\n\nEnter the file name fron which data is to be read";	--getting input for filename from which data is to be read
	filename <- getLine;
	putStrLn ("\n\n")

	let list_main = []							--creating two list one of Int other of String
	let list_main2 = []


        
	let modified_list=[]							--temperory list

        globl<-newIORef(0::Int)							--global variable for concurrency hadling during sorting

	let loop= do
		
										--asking for user choice
		    putStrLn "Press 1 to search for a query and get the queries accessed in recent past\n";
		    putStrLn "Press 2 to search a query using substring/stringlength matching\n";
		    putStrLn "Press 3 to search for indices of the query in file\n";
		    putStrLn "Press 4 to delete some query from history\n";
		    putStrLn "Press 5 to to sort the input data in ascending order of magnitude\n";
		    putStrLn "Press 6 to estimate rough frequency of the query \n";	
	            putStrLn "Press 7 to exit\n";


		    handle <- openFile filename ReadMode		-- handle - open file in read mode and take the input into list of int
                    contents <- hGetContents handle

                    let singlewords = words contents

                   	list_main = f1 singlewords

		    forceList contents `seq` hClose handle	        --close the file after reading
		    
		    choice <- getNumber;

		    
			   if choice==1
	        		then do
					putStrLn ("Enter the query\n")		--asking user input
					search_query <-getNumber	
							
								--creating a splay tree from the list
					let splaytree= list_to_splay list_main	

								--new splaytree with the new element inserted
					let splaytree1 = insert_splay search_query splaytree 
				
								--search for the element and splay it up
					let modified_tree= seek search_query splaytree1  

								--add the newly searched element to the list
					let modified_list=add_element search_query list_main



					putStrLn ("Maybe you are searching for one of the following queries\n")
					
							
							--printing the root and its nearby 6 children which are the most recently accessed
							--in case the node is not present print null else print the value at node

					if (True) then do
						  (print $ root (modified_tree))
  						  `catch` (\(ErrorCall msg) -> putStrLn "Null")
  						  `catch` (\(exc::SomeException) -> putStrLn "Null")
						  
					else print ("\t")

					
					if (True) then do
						  (print $ root (rightchild(modified_tree)))
  						  `catch` (\(ErrorCall msg) -> putStrLn "Null")
  						  `catch` (\(exc::SomeException) -> putStrLn "Null")
						  
					else print ("\t")


					if (True) then do
						  (print $ root (leftchild(modified_tree)))
  						  `catch` (\(ErrorCall msg) -> putStrLn "Null")
  						  `catch` (\(exc::SomeException) -> putStrLn "Null")
						  
					else print ("\t")


					if (True) then do
						  (print $ root (rightchild(leftchild(modified_tree))))
  						  `catch` (\(ErrorCall msg) -> putStrLn "Null")
  						  `catch` (\(exc::SomeException) -> putStrLn "Null")
						  
					else print ("\t")


					if (True) then do
						  (print $ root (leftchild (rightchild(modified_tree))))
  						  `catch` (\(ErrorCall msg) -> putStrLn "Null")
  						  `catch` (\(exc::SomeException) -> putStrLn "Null")
						  
					else print ("\t")


					if (True) then do
						  (print $ root (leftchild(leftchild(modified_tree))))
  						  `catch` (\(ErrorCall msg) -> putStrLn "Null")
  						  `catch` (\(exc::SomeException) -> putStrLn "Null")
						  
					else print ("\t")


					if (True) then do
						  (print $ root (rightchild (rightchild(modified_tree))))
  						  `catch` (\(ErrorCall msg) -> putStrLn "Null")
  						  `catch` (\(exc::SomeException) -> putStrLn "Null")
						  
					else putStrLn ("")



				        let lists=f' modified_list	
					outh <- openFile filename WriteMode
					hPutStrLn outh (unlines lists)
    					hClose outh


					
	      			else putStrLn ("");

						    --input 2 asks user if he wants to search based 
						    --on the length/start character/or substrings of the records(big data file)
			   if choice==2
				then do
						    --file opened for search and stored in a list called list_main2

				        handle2 <- openFile filename ReadMode
				        contents2 <- hGetContents handle2
					let list_main2 = words contents2
					
						    --asks user for sub_choices

					putStrLn ("Press 'a' to search for queries containing a substring\n")
					putStrLn ("Press 'b' to search for queries starting with substring\n")
					putStrLn ("Press 'c' to search for queries of particular string length\n")
					
					sub_choice <- getLine; 
						
						if (sub_choice=="a")
							then do				
								putStrLn ("Enter the substring to search for\n")
								get_index <- getLine

									--prints those elements in the file which has specified input as 										--substring

								let indices_substring =containing_sub list_main2 get_index
								putStrLn ("You might want to search for");
								mapM_ print (indices_substring)

						else if (sub_choice=="b")
							then do				
								putStrLn ("Enter the substring to search for\n")
								get_index <- getLine

									--prints those elements in the file which has specified input as 										--starting substring 

								let indices_substring =start_string list_main2 get_index
								putStrLn ("You might want to search for");
								mapM_ print (indices_substring)

						else if (sub_choice=="c")
							then do				
								putStrLn ("Enter the stringlength to search for\n")
								get_index <- getNumber

									--prints those elements in the file which has specified length as input

								let indices_strlen =strlen_search list_main2 get_index
								putStrLn ("You might want to search for");
								mapM_ print (indices_strlen)
							else
								putStrLn ("");
							

									--forcefully closes the list to escape from resource busy and lazy 										--binding problems
					                forceList contents `seq` hClose handle2


				else putStrLn ("") ;

		    							--removes a query from the big data file forever

			   if choice==3
				then do
					putStrLn ("Enter the query of which index is to be found\n")
					get_index <- getNumber
					let indices=find_index list_main (get_index)
					print ("The line number in files matching the query are\n")
					mapM_ print (indices)

				else putStrLn ("") ;

			   if choice==4
				then do
					putStrLn ("Enter the query to be removed fromt the history\n")
					get_index <- getNumber
					let new_list= delete_from_list (get_index) list_main 
					let lists=f' new_list
							
					outh <- openFile filename WriteMode
					hPutStrLn outh (unlines lists)
    					hClose outh
							
					print ("Succesfully deleted\n")

				else putStrLn ("") ;

		    

		    
			   if choice==5
				then do
					print ("Sorted sequence is\n")	
							--globl variable made 0 to in each iteration so that it helps in 								--synchronization					
					modifyIORef globl (0*)

							--both the two sorts are run concurrently 
					forkIO (ms(list_main,globl))	
					forkIO (qs(list_main,globl))

							--as defined in function ms and qs only that one prints the output which is ready with 								--the result first
							--since data can be of different sizes the main function waits for its threads to 								--complete accordingly
			
					threadDelay (500*length(list_main))

				else putStrLn (""); 

		    
		    
			   if choice==6
	        		then do
							--frequency estimator(implemented using count_min sketch) is called with the input file 							--as argument

				print ("Frequency estimator")
				forceList contents `seq` hClose handle
	        		callProcess "./count_min" [filename]			
		
	      			else putStrLn ("");

			   if (choice>=8 || choice<=0)
	        		then print ("Invalid input\n")
	      			else putStrLn ("");

			    
							--choice 7 is used for gracefully aborting the execution of the program
       	 	    when (choice/=(7)) loop

				
        loop
