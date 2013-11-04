{--
	MARIN CRISTIAN-EMIL
	
	Implementarea SmartArray in paradigma functionala
--}

module SmartArray where


	add :: [Int] -> Int -> [Int]
	add [] x = [x]
	add xs x = reverse $ x : reverse xs
	
	
	delete :: [Int] -> Int -> [Int]
	delete [] _ = []
	delete (x:xs) val
			|	x == val  = delete xs val
			| 	x  /= val = x : (delete xs val)
	
	
	pairs :: [Int] -> [(Int, Int)]
	pairs [] = []
	pairs (x:xs)
			|	(-x) `elem` sorted_neg xs = (x,-x)  : pairs xs
			| 	otherwise 		  = pairs xs
	
	
	zerosum :: [Int] -> [[Int]]
	zerosum = (filtersubs.tail) . subsets
	
	-- Functii pentru pairs --
	
	-- functie care filtreaza elementele negative dintr-o lista
	neg :: [Int] -> [Int]
	neg [] = []
	neg xs = filter (<0) xs
	
	-- functie care filtreaza elementele pozitive dintr-o lista
	poz :: [Int] -> [Int]
	poz [] = []
	poz xs = filter (>0) xs
	
	-- Algoritmul quick-sort implementat pentru sortarea unei liste de elemente de tip 'a'
	qsort :: (Ord a) => [a] -> [a]
	qsort [] = []
	qsort (x:xs) =
			let 
			  lesser = filter ( <  x ) (qsort xs)
			  greater = filter ( >= x ) (qsort xs)
			in lesser ++ [x] ++ greater
	
	-- functie care sorteaza lista de numere negative
	sorted_neg :: [Int] -> [Int]
	sorted_neg [] = []
	sorted_neg xs = qsort $ neg xs
	
	-- functie care sorteaza lista de numere pozitive
	sorted_poz :: [Int] -> [Int]
	sorted_poz [] = []
	sorted_poz xs = qsort $ poz xs
	
	-- Functii pentru zerosum
	
	-- functie de generare a tuturor subseturilor unui set ( => 2^n subseturi, unde n == lungimea setului)
	subsets :: [Int] -> [[Int]]
	subsets []  = [[]]
	subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)
	
	-- predicat care intoarce True daca suma elementelor dintr-o lista este egala cu zero
	sumzero :: [Int] -> Bool
	sumzero xs = sum xs == 0
	
	-- functie care primeste o lista de liste si intoarce tot o lista de liste, dar filtrata cu predicatul `sumzero`
	filtersubs :: [[Int]] -> [[Int]]
	filtersubs = filter sumzero
	
