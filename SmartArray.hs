{--
	MARIN CRISTIAN-EMIL
	
	Implementarea SmartArray in paradigma functionala
	
	OBSERVATII: 
			-> sursa am testat-o folosind interpretorul Prelude din GHCi pe Windows ( http://www.haskell.org/platform/windows.html)
			-> metoda de testare: se intra in cmd, apoi "cd /calea_catre_fisier/" si se executa comanda "ghci". Din prelude se foloseste comanda: ":load SmartArray.hs" si apoi 
se testeaza fiecare functie(add, delete, pairs, zerosum) utilizand o lista de test. Exemplu: delete [-3,6,5,3,10,6,2,3,9,-3,-2,-5] (-3) .	Am scris numarul -3 in paranteza pentru
ca altfel aparea eroare de tip.
--}

module SmartArray where


	add :: [Int] -> Int -> [Int]
	add [] x = [x]
	add xs x = reverse $ x : reverse xs
	
	
	delete :: [Int] -> Int -> [Int]
	delete [] _ = []
	delete (x:xs) val
			|	x == val = delete xs val
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
	
