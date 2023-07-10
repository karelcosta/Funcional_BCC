import Data.List (sortBy)
import Data.Ord (comparing)

data Pessoa = Pessoa
  { nome :: String
  , idade :: Int
  , endereco :: Endereco
  } deriving (Eq, Show)

data Endereco = Endereco
  { rua :: String
  , numero :: Int
  , cidade :: String
  } deriving (Eq, Show)

type Cadastro = [Pessoa]

menu :: Cadastro -> IO ()
menu dados = do
  putStrLn "\n--------MENU--------"
  putStrLn "Digite 1 para cadastrar pessoa"
  putStrLn "Digite 2 para pesquisar pessoa"
  putStrLn "Digite 3 para imprimir cidades"
  putStrLn "Digite 0 para sair"
  putStr "Opção: "
  opt <- getChar
  getChar -- descarta o Enter
  case opt of
    '0' -> do
      putStrLn "\n--------FIM--------"
    '1' -> do
      novoCadastro <- atualizar dados
      menu novoCadastro
    '2' -> do
      novoCadastro <- pesquisarPessoaMenu dados
      menu novoCadastro
    _ -> do
      putStrLn "\nOpção inválida!"
      menu dados

main :: IO ()
main = do
  menu []
  return ()

atualizar :: Cadastro -> IO Cadastro
atualizar cadastro = do
  pessoa <- cadastrar
  let nomePessoa = nome pessoa
      novoCadastro = removerPessoa cadastro nomePessoa
      novoCadastroOrdenado = ordenaPessoas (pessoa : novoCadastro)
  putStrLn "Pessoa adicionada com sucesso!"
  return novoCadastroOrdenado

ordenaPessoas :: Cadastro -> Cadastro
ordenaPessoas = sortBy (comparing nome)

pesquisar :: Cadastro -> String -> Maybe Pessoa
pesquisar [] _ = Nothing
pesquisar (p : ps) nomePessoa
  | nome p == nomePessoa = Just p
  | otherwise = pesquisar ps nomePessoa

cadastrar :: IO Pessoa
cadastrar = do
  putStrLn "Digite o nome da pessoa: "
  nome <- getLine
  putStrLn "Digite a idade da pessoa: "
  idade <- readLn
  putStrLn "Digite a rua do endereço: "
  rua <- getLine
  putStrLn "Digite o número do endereço: "
  numero <- readLn
  putStrLn "Digite a cidade do endereço: "
  cidade <- getLine
  return (Pessoa nome idade (Endereco rua numero cidade))

removerPessoa :: Cadastro -> String -> Cadastro
removerPessoa [] _ = []
removerPessoa (p : ps) nomePessoa
  | nome p == nomePessoa = ps
  | otherwise = p : removerPessoa ps nomePessoa

pesquisarPessoaMenu :: Cadastro -> IO Cadastro
pesquisarPessoaMenu cadastro = do
  putStrLn "Digite o nome da pessoa a ser pesquisada: "
  nomePessoa <- getLine
  let resultado = pesquisar cadastro nomePessoa
  case resultado of
    Just pessoa -> do
      putStrLn ("Pessoa encontrada: " ++ show pessoa)
      return cadastro
    Nothing -> do
      putStrLn "Pessoa não encontrada."
      return cadastro
