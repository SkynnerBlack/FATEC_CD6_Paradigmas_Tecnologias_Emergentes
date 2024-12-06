{- -- Quest 4.3
-- Filtra pares
filtraPares :: [Int] -> [Int]
filtraPares = filter even

-- Filtra ímpares
filtraImpares :: [Int] -> [Int]
filtraImpares = filter odd

-- Função principal
main :: IO ()
main = do
    let lista = [1..10]
    putStrLn $ "Lista original: " ++ show lista
    putStrLn $ "Pares: " ++ show (filtraPares lista)
    putStrLn $ "Ímpares: " ++ show (filtraImpares lista)
-}

{- -- Quest 4.4
ehPrimo :: Int -> Bool
ehPrimo n = null [x | x <- [2..n-1], n `mod` x == 0]

filtrarPrimos :: [Int] -> [Int]
filtrarPrimos = filter ehPrimo

-- Função principal
main :: IO ()
main = do
    let lista = [1..10]
    putStrLn $ "Lista original: " ++ show lista
    putStrLn $ "Primos: " ++ show (filtrarPrimos lista)
-}

{- -- Quest 4.6
reverter :: String -> String
reverter [] = []
reverter (x:xs) = reverter xs ++ [x]

main :: IO ()

palavraOriginal :: String
palavraOriginal = "Haskell"

main = do
    putStrLn $ "Palavra original: " ++ show palavraOriginal
    putStrLn $ "Palavra invertida: " ++ show (reverter palavraOriginal)
-}

{- -- Quest 4.7

-- Define um tipo enumerado para os dias da semana
data Dia = Segunda | Terca | Quarta | Quinta | Sexta | Sabado | Domingo
    deriving (Eq, Show)

-- Função para filtrar as terças-feiras
filtrarTercas :: [Dia] -> [Dia]
filtrarTercas = filter (== Terca)

-- Lista de dias
dias :: [Dia]
dias = [Segunda, Terca, Quarta, Quinta, Sexta, Sabado, Domingo]

-- Função principal
main :: IO ()
main = do
    putStrLn $ "Lista original: " ++ show dias
    putStrLn $ "Lista filtrada: " ++ show (filtrarTercas dias)
-}

{- -- Quest 4.8
-- Define um tipo para representar Dinheiro em Real ou Dólar
data Dinheiro = Real Double | Dolar Double deriving (Show)

-- Função para converter Real para Dólar
converterParaDolar :: Dinheiro -> Dinheiro
converterParaDolar (Real valor) = Dolar (valor / 5)
converterParaDolar (Dolar valor) = Dolar valor

-- Função para converter uma lista de Dinheiro para Dólar
converterListaParaDolar :: [Dinheiro] -> [Dinheiro]
converterListaParaDolar = map converterParaDolar

-- Função para converter Real para Dólar
converterParaReal :: Dinheiro -> Dinheiro
converterParaReal (Real valor) = Real valor
converterParaReal (Dolar valor) = Real (valor * 5)

-- Função para converter uma lista de Dinheiro para Real
converterListaParaReal :: [Dinheiro] -> [Dinheiro]
converterListaParaReal = map converterParaReal

-- Função para filtrar apenas os Dólares de uma lista
filtrarDolares :: [Dinheiro] -> [Dinheiro]
filtrarDolares = filter ehDolar
    where
        ehDolar (Dolar _) = True
        ehDolar _ = False

-- Função para somar os valores em Dólar de uma lista
somarDolares :: [Dinheiro] -> Double
somarDolares xs = foldl somarValoresDolar 0 (filtrarDolares xs)
    where
        somarValoresDolar acc (Dolar valor) = acc + valor
        somarValoresDolar acc _ = acc

-- Função para contar a quantidade de Dólares em uma lista
contarDolares :: [Dinheiro] -> Int
contarDolares xs = foldl contarValoresDolar 0 (filtrarDolares xs)
    where
        contarValoresDolar acc (Dolar _) = acc + 1
        contarValoresDolar acc _ = acc

-- Função principal para testar as funções acima
main :: IO ()
main = do
    let listaDinheiro = [Real 10.0, Dolar 3.0, Real 20.0, Dolar 5.0]
    
    putStrLn $ "Lista original: " ++ show listaDinheiro
    putStrLn $ "Lista convertida para Dólar: " ++ show (converterListaParaDolar listaDinheiro)
    putStrLn $ "Lista convertida para Real: " ++ show (converterListaParaReal listaDinheiro)
    putStrLn $ "Somatório dos valores em Dólar: " ++ show (somarDolares listaDinheiro)
    putStrLn $ "Quantidade de Dólares: " ++ show (contarDolares listaDinheiro)
-}

{- -- Quest 4.9
-- Define um tipo para os dias da semana
data DiaSemana = Domingo | Segunda | Terca | Quarta | Quinta | Sexta | Sabado
    deriving (Show, Eq)

-- Função para contar quantos números negativos há em uma lista
contarNegativos :: [Int] -> Int
contarNegativos = foldl (\acc x -> if x < 0 then acc + 1 else acc) 0

-- Função para contar quantas vezes a letra 'P' aparece em uma string
contarLetrasP :: String -> Int
contarLetrasP = foldl (\acc x -> if x == 'P' then acc + 1 else acc) 0

-- Função para contar quantos sábados há em uma lista de dias
contarSabados :: [DiaSemana] -> Int
contarSabados = foldl (\acc dia -> if dia == Sabado then acc + 1 else acc) 0

-- Função para converter um dia da semana em um número inteiro
diaParaInt :: DiaSemana -> Int
diaParaInt Domingo = 1
diaParaInt Segunda = 2
diaParaInt Terca = 3
diaParaInt Quarta = 4
diaParaInt Quinta = 5
diaParaInt Sexta = 6
diaParaInt Sabado = 7

-- Função para somar os valores inteiros correspondentes aos dias de uma lista de dias
somaDias :: [DiaSemana] -> Int
somaDias = foldl (\acc dia -> acc + diaParaInt dia) 0

-- Função principal para testar as funções acima
main :: IO ()
main = do
    let numeros = [-1, 2, -3, 4, -5]
    let texto = "PPPPPPQQQPPP"
    let diasSemana = [Segunda, Sabado, Quarta, Sabado, Sexta, Sabado]

    putStrLn $ "Lista de números: " ++ show numeros
    putStrLn $ "Quantidade de negativos: " ++ show (contarNegativos numeros)

    putStrLn $ "Texto: " ++ texto
    putStrLn $ "Quantidade de letras 'P': " ++ show (contarLetrasP texto)

    putStrLn $ "Lista de dias da semana: " ++ show diasSemana
    putStrLn $ "Quantidade de sábados: " ++ show (contarSabados diasSemana)

    putStrLn $ "Soma dos valores inteiros dos dias da semana: " ++ show (somaDias diasSemana)
-}