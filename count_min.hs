import Data.Array.IO					--imported for IO array operations (mutable arrays without changing it as a whole)
import Control.Monad				
import System.IO 
import Data.List (insert)
import Data.Bool
import Data.IORef					--imported for using global variables 
import System.Environment
					
{-#LANGUAGE ScopedTypeVariables#-}

f :: [String] -> [Int]						--function to convert string list to int list
f = map read

f' :: [Int] -> [String]						--function to convert int list to string list
f' = (>>= return.show)

getNumber::IO Int						--function which takes input as string and return int 
getNumber = do
    num <- getLine
    return (read num)

main = do
	let list = []						--list which keeps all the elements of the big data file in it
        
        arr1 <- newArray (0,1000) 0 :: IO (IOArray Int Int)	--first of all the four arrays required in count-min sketch data  structure
        arr2 <- newArray (0,1000) 0 :: IO (IOArray Int Int)	--second of all the four arrays required in count-min sketch data  structure
        arr3 <- newArray (0,1000) 0 :: IO (IOArray Int Int)      --third of all the four arrays required in count-min sketch data  structure
        arr4 <- newArray (0,1000) 0 :: IO (IOArray Int Int)      --fourth of all the four arrays required in count-min sketch data  structure
        
 	[filename]<-getArgs							--file handling      
        handle <- openFile filename ReadMode			
 	contents <- hGetContents handle				--contents is a list which reads the file and store all the elements of it
        let singlewords = words contents			--singlewords is the list which stores each word of the file in the list separately
            list = f singlewords				--converts the string list into integer list	
    
        let lists=(list++[-1])					--lists stores the list plus -1 appended required for further manipulation.However it does not effect the output in any ways.Just used for halting .
        i<-newIORef(0::Int)					--global variable which acts as a counter
 
        let loop= do						--loop begins

        	   out<-readIORef i				--out stores the value of i as an ioref int
        	   let n= (lists!!out)				--number which needs to be hashed 

       	 	   let hash_cell= mod (n*5) 1001			--hash1
       	 	   a <- readArray arr1 hash_cell
       	 	   let incremented_a=a+1					--increments the current value of hash cell by 1
	 	   writeArray arr1 hash_cell incremented_a
	 	   
	 	   let hash_cell= mod (n*7) 1001			--hash2
	 	   a <- readArray arr2 hash_cell
	 	   let incremented_a=a+1
	 	   writeArray arr2 hash_cell incremented_a
	 	   
	 	   let hash_cell= mod (n*13) 1001			--hash3
	 	   a <- readArray arr3 hash_cell
	 	   let incremented_a=a+1
	 	   writeArray arr3 hash_cell incremented_a
	 	   
	 	   let hash_cell= mod (n*21) 1001			--hash4
	 	   a <- readArray arr4 hash_cell
	 	   let incremented_a=a+1
	 	   writeArray arr4 hash_cell incremented_a
	 	  
	 	   modifyIORef i (+1)				--increments counter
       	 	   when (n/=(-1)) loop
        loop  							--loop ends
        
	putStrLn "Enter a number to find its frequency!"		
	
	n <- getNumber						--scans the number from user for which frequency is to be estimated
	let hash_1=mod (n*5) 1001					
	
	min <- readArray arr1 hash_1					--assumes the minimum hashed value of the n in the first hash table 
	
	let hash_2=mod (n*7) 1001					--moves on to check in hash table2
	hash_value_2 <- readArray arr2 hash_2
	
	let hash_3=mod (n*13) 1001					--moves on to check in hash table3 
	hash_value_3 <- readArray arr3 hash_3
	
	let hash_4=mod (n*21) 1001					--moves on to check in hash table4
	hash_value_4 <- readArray arr4 hash_4
	
	putStrLn "Frequency is:!"
	
	let ab=							 --records if the second hash value is minimum
		if min>hash_value_2
	        	then hash_value_2
	      		else min
	
	let bc=
		if ab>hash_value_3				--records if the third hash value is minimum
	        	then hash_value_3
	      		else ab
	
	let cd=
		if bc>hash_value_4				--records if the fourth hash value is minimum
	        	then hash_value_4
	      		else bc

	print cd						--minimum value so found
