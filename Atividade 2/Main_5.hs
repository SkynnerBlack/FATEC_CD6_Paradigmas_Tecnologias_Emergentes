{- -- Quest 5.1
data TipoProduto = Escritorio | Informatica | Livro | Filme | Total deriving (Show, Eq)

data Produto = Produto { valor :: Double, tipo :: TipoProduto } | Nulo deriving (Show, Eq)

instance Semigroup Produto where
    (<>) :: Produto -> Produto -> Produto
    Nulo <> p = p
    p <> Nulo = p
    Produto v1 _ <> Produto v2 _ = Produto (v1 + v2) Total


instance Monoid Produto where
    mempty :: Produto
    mempty = Nulo

main :: IO ()
main = do
    let p1 = Produto 100.0 Informatica
    let p2 = Produto 50.0 Livro
    let p3 = Produto 30.0 Filme
    let nulo = Nulo
    
    -- Testando a combinação de dois produtos
    putStrLn "Combinação de dois produtos (Informatica e Livro):"
    print (p1 <> p2)
    
    -- Testando a combinação de produto com Nulo
    putStrLn "\nCombinação de produto com Nulo (Informatica e Nulo):"
    print (p1 <> nulo)
    
    -- Testando a combinação de três produtos
    putStrLn "\nCombinação de três produtos (Informatica, Livro e Filme):"
    print (p1 <> p2 <> p3)
-}

{- -- Quest 5.2
-- Define um tipo Produto
data Produto = Produto Double String | Nulo
    deriving (Show)

-- Torna Produto uma instância de Monoid
instance Semigroup Produto where
    Nulo <> p = p
    p <> Nulo = p
    Produto v1 n1 <> Produto v2 n2 = Produto (v1 + v2) (n1 ++ ", " ++ n2)

instance Monoid Produto where
    mempty = Nulo

-- Função para calcular o total geral de uma lista de produtos
totalGeral :: [Produto] -> Double
totalGeral produtos = case mconcat produtos of
    Nulo -> 0.0  -- Se o resultado for Nulo, o total é 0
    Produto v _ -> v  -- Se for um Produto, retorne o valor

-- Função principal para testar o comportamento
main :: IO ()
main = do
    let produtos = [Produto 10.0 "Caneta", Produto 20.0 "Lápis", Produto 15.0 "Caderno", Nulo]
    
    putStrLn $ "Lista de produtos: " ++ show produtos
    putStrLn $ "Total geral: " ++ show (totalGeral produtos)
-}

{- -- Quest 5.3
-- Define o tipo Min
data Min = Min Int deriving (Show, Eq)

-- Torna Min uma instância de Ord para permitir comparação
instance Ord Min where
    (Min x) <= (Min y) = x <= y

-- Torna Min uma instância de Semigroup
instance Semigroup Min where
    (Min x) <> (Min y) = Min (min x y)

-- Torna Min uma instância de Monoid
instance Monoid Min where
    mempty = Min maxBound

-- Função para calcular o mínimo de uma lista de inteiros
calcularMinimo :: [Int] -> Int
calcularMinimo xs = let Min resultado = mconcat (map Min xs) in resultado

-- Função principal para testar o comportamento
main :: IO ()
main = do
    let numeros = [10, 3, 45, 2, 8, 12]
    putStrLn $ "Lista de números: " ++ show numeros
    putStrLn $ "Menor número da lista: " ++ show (calcularMinimo numeros)
-}

{- -- Quest 5.4
-- Define o tipo Min
data Min = Min Int deriving (Show, Eq)

-- Torna Min uma instância de Ord para permitir comparação
instance Ord Min where
    (Min x) <= (Min y) = x <= y

-- Torna Min uma instância de Semigroup
instance Semigroup Min where
    (Min x) <> (Min y) = Min (min x y)

-- Torna Min uma instância de Monoid
instance Monoid Min where
    mempty = Min maxBound

-- Função para calcular o menor valor de uma lista de Min
minAll :: [Min] -> Min
minAll [] = mempty
minAll xs = foldl mappend mempty xs

-- Função principal para testar o comportamento
main :: IO ()
main = do
    let valores = [Min 10, Min 3, Min 45, Min 2, Min 8, Min 12]
    putStrLn $ "Lista de valores Min: " ++ show valores
    putStrLn $ "Menor valor: " ++ show (minAll valores)
-}

{- -- Quest 5.5
data Paridade = Par | Impar deriving (Show, Eq)

class ParImpar a where 
    decide :: a -> Paridade

instance ParImpar Int where 
    decide n
        | even n    = Par
        | otherwise = Impar

instance ParImpar [a] where 
    decide xs
        | even (length xs) = Par
        | otherwise        = Impar

instance ParImpar Bool where
    decide False = Par
    decide True  = Impar

-- Classe para calcular a média
class Media a where
    mean :: a -> Double

-- Instância para listas de números
instance Media [Double] where
    mean xs
        | null xs   = 0  -- Caso a lista esteja vazia
        | otherwise = sum xs / fromIntegral (length xs)

-- Exemplo de uso
main :: IO ()
main = do
    -- Testando a função decide
    print $ decide (5 :: Int)           -- Saída: Impar
    print $ decide [1, 2, 3, 4]         -- Saída: Par
    print $ decide False                -- Saída: Par

    -- Testando a função mean
    print $ mean ([1.0, 2.0, 3.0, 4.0] :: [Double])   -- Saída: 2.5
    print $ mean ([] :: [Double])                     -- Saída: 0.0
-}

{- -- Quest 5.7
data Arvore a = Folha a | No (Arvore a) (Arvore a) deriving (Show)

-- Função mapa que aplica uma função a todos os elementos da árvore
mapa :: (a -> b) -> Arvore a -> Arvore b
mapa f (Folha x) = Folha (f x)
mapa f (No esquerda direita) = No (mapa f esquerda) (mapa f direita)

-- Exemplo de uso
main :: IO ()
main = do
    let arvore = No (Folha 1) (No (Folha 2) (Folha 3)) -- Árvore de exemplo
    print arvore                                      -- Saída: No (Folha 1) (No (Folha 2) (Folha 3))
    
    let dobrada = mapa (*2) arvore                    -- Mapeando a função (*2)
    print dobrada                                     -- Saída: No (Folha 2) (No (Folha 4) (Folha 6))
    
    let paraString = mapa show arvore                 -- Convertendo elementos para String
    print paraString                                  -- Saída: No (Folha "1") (No (Folha "2") (Folha "3"))
-}

-- {- -- Quest 5.8
somar5 :: Int -> Int
somar5 x = x + 5

main :: IO ()
main = do
    let resultado = somar5 10
    putStrLn ("O resultado da soma é: " ++ show resultado)
-- -}

{- -- Quest 5.11
-}