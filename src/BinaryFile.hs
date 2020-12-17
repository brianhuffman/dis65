module BinaryFile where

import qualified Data.ByteString as B

import Data.Char (ord)
import Data.Word (Word8, Word16)
import Data.Int (Int8)
import Data.Array

ascii :: String -> [Word8]
ascii = map (fromIntegral . ord)

crt8k :: String -> [Word8]
crt8k name =
  ascii "C64 CARTRIDGE   "
  ++ [0,0,0,64,1,0,0,0,0,1,0,0,0,0,0,0]
  ++ take 32 (ascii name ++ repeat 0)
  ++ ascii "CHIP"
  ++ [0,0,32,16,0,0,0,0,128,0,32,0]

crt16k :: String -> [Word8]
crt16k name =
  ascii "C64 CARTRIDGE   "
  ++ [0,0,0,64,1,0,0,0,0,0,0,0,0,0,0,0]
  ++ take 32 (ascii name ++ repeat 0)
  ++ ascii "CHIP"
  ++ [0,0,64,16,0,0,0,0,128,0,64,0]

readBytes :: FilePath -> IO [Word8]
readBytes = fmap B.unpack . B.readFile

writeBytes :: FilePath -> [Word8] -> IO ()
writeBytes path = B.writeFile path . B.pack

showByte :: Word8 -> String
showByte b = [ds !! hi, ds !! lo]
  where (hi, lo) = divMod (fromIntegral b) 16
        ds = "0123456789abcdef"

showWord :: Word16 -> String
showWord w = showByte hi ++ showByte lo
      where hi = fromIntegral (w `div` 0x100)
            lo = fromIntegral w

prettyBytes :: [Word8] -> String
prettyBytes [] = ""
prettyBytes ws = "\n " ++ concatMap f xs ++ prettyBytes ys
  where (xs, ys) = splitAt 16 ws
        f x = " 0x" ++ showByte x ++ ","

decodeBytes :: Word16 -> [Word8] -> String
decodeBytes _addr [] = ""
decodeBytes addr (op : bytes) =
  case (opTable ! op, bytes) of
    (Imp n, xs) -> pre n ++ "         ; " ++ post [] xs
    (Imm n, x : xs) -> pre n ++ " #$" ++ showByte x ++ "    ; " ++ post [x] xs
    (Zpg n, x : xs) -> pre n ++ " $" ++ showByte x ++ "     ; " ++ post [x] xs
    (Zpx n, x : xs) -> pre n ++ " $" ++ showByte x ++ ",X   ; " ++ post [x] xs
    (Zpy n, x : xs) -> pre n ++ " $" ++ showByte x ++ ",Y   ; " ++ post [x] xs
    (Abs n, x : y : xs) -> pre n ++ " $" ++ showByte y ++ showByte x ++ "   ; " ++ post [x,y] xs
    (Abx n, x : y : xs) -> pre n ++ " $" ++ showByte y ++ showByte x ++ ",X ; " ++ post [x,y] xs
    (Aby n, x : y : xs) -> pre n ++ " $" ++ showByte y ++ showByte x ++ ",Y ; " ++ post [x,y] xs
    (Ind n, x : y : xs) -> pre n ++ " ($" ++ showByte y ++ showByte x ++ ") ; " ++ post [x,y] xs
    (Inx n, x : xs) -> pre n ++ " ($" ++ showByte x ++ ",X) ; " ++ post [x] xs
    (Iny n, x : xs) -> pre n ++ " ($" ++ showByte x ++ "),Y ; " ++ post [x] xs
    (Rel n, x : xs) -> pre n ++ " $" ++ showRel x ++ "   ; " ++ post [x] xs
    _ -> showWord addr ++ ": byte " ++ showByte op ++ "\n" ++ decodeBytes (addr + 1) bytes
  where
    pre n = showWord addr ++ ": " ++ n
    post args xs = unwords (map showByte (op : args)) ++ "\n" ++
                   decodeBytes (addr + 1 + fromIntegral (length args)) xs
    showRel x = showWord (addRel (addr + 2) x)

addRel :: Word16 -> Word8 -> Word16
addRel base offset = base + fromIntegral (fromIntegral offset :: Int8)

data Op
  = Imp String
  | Imm String
  | Zpg String
  | Zpx String
  | Zpy String
  | Abs String
  | Abx String
  | Aby String
  | Ind String
  | Inx String
  | Iny String
  | Rel String
  | Undoc

--data RelOp = BPL | BMI | BVC | BVS | BCC | BCS | BNE | BEQ
--  deriving (Eq, Show)


opTable :: Array Word8 Op
opTable = listArray (0x00, 0xff)
  [ Imp "BRK", Inx "ORA", Undoc    , Undoc
  , Undoc    , Zpg "ORA", Zpg "ASL", Undoc
  , Imp "PHP", Imm "ORA", Imp "ASL", Undoc
  , Undoc    , Abs "ORA", Abs "ASL", Undoc
  , Rel "BPL", Iny "ORA", Undoc    , Undoc
  , Undoc    , Zpx "ORA", Zpx "ASL", Undoc
  , Imp "CLC", Aby "ORA", Undoc    , Undoc
  , Undoc    , Abx "ORA", Abx "ASL", Undoc
  , Abs "JSR", Inx "AND", Undoc    , Undoc
  , Zpg "BIT", Zpg "AND", Zpg "ROL", Undoc
  , Imp "PLP", Imm "AND", Imp "ROL", Undoc
  , Abs "BIT", Abs "AND", Abs "ROL", Undoc
  , Rel "BMI", Iny "AND", Undoc    , Undoc
  , Undoc    , Zpx "AND", Zpx "ROL", Undoc
  , Imp "SEC", Aby "AND", Undoc    , Undoc
  , Undoc    , Abx "AND", Abx "ROL", Undoc
  , Imp "RTI", Inx "EOR", Undoc    , Undoc
  , Undoc    , Zpg "EOR", Zpg "LSR", Undoc
  , Imp "PHA", Imm "EOR", Imp "LSR", Undoc
  , Abs "JMP", Abs "EOR", Abs "LSR", Undoc
  , Rel "BVC", Iny "EOR", Undoc    , Undoc
  , Undoc    , Zpx "EOR", Zpx "LSR", Undoc
  , Imp "CLI", Aby "EOR", Undoc    , Undoc
  , Undoc    , Abx "EOR", Abx "LSR", Undoc
  , Imp "RTS", Inx "ADC", Undoc    , Undoc
  , Undoc    , Zpg "ADC", Zpg "ROR", Undoc
  , Imp "PLA", Imm "ADC", Imp "ROR", Undoc
  , Ind "JMP", Abs "ADC", Abs "ROR", Undoc
  , Rel "BVS", Iny "ADC", Undoc    , Undoc
  , Undoc    , Zpx "ADC", Zpx "ROR", Undoc
  , Imp "SEI", Aby "ADC", Undoc    , Undoc
  , Undoc    , Abx "ADC", Abx "ROR", Undoc
  , Undoc    , Inx "STA", Undoc    , Undoc
  , Zpg "STY", Zpg "STA", Zpg "STX", Undoc
  , Imp "DEY", Undoc    , Imp "TXA", Undoc
  , Abs "STY", Abs "STA", Abs "STX", Undoc
  , Rel "BCC", Iny "STA", Undoc    , Undoc
  , Zpx "STY", Zpx "STA", Zpy "STX", Undoc
  , Imp "TYA", Aby "STA", Imp "TXS", Undoc
  , Undoc    , Abx "STA", Undoc    , Undoc
  , Imm "LDY", Inx "LDA", Imm "LDX", Undoc
  , Zpg "LDY", Zpg "LDA", Zpg "LDX", Undoc
  , Imp "TAY", Imm "LDA", Imp "TAX", Undoc
  , Abs "LDY", Abs "LDA", Abs "LDX", Undoc
  , Rel "BCS", Iny "LDA", Undoc    , Undoc
  , Zpx "LDY", Zpx "LDA", Zpy "LDX", Undoc
  , Imp "CLV", Aby "LDA", Imp "TSX", Undoc
  , Abx "LDY", Abx "LDA", Aby "LDX", Undoc
  , Imm "CPY", Inx "CMP", Undoc    , Undoc
  , Zpg "CPY", Zpg "CMP", Zpg "DEC", Undoc
  , Imp "INY", Imm "CMP", Imp "DEX", Undoc
  , Abs "CPY", Abs "CMP", Abs "DEC", Undoc
  , Rel "BNE", Iny "CMP", Undoc    , Undoc
  , Undoc    , Zpx "CMP", Zpx "DEC", Undoc
  , Imp "CLD", Aby "CMP", Undoc    , Undoc
  , Undoc    , Abx "CMP", Abx "DEC", Undoc
  , Imm "CPX", Inx "SBC", Undoc    , Undoc
  , Zpg "CPX", Zpg "SBC", Zpg "INC", Undoc
  , Imp "INX", Imm "SBC", Imp "NOP", Undoc
  , Abs "CPX", Abs "SBC", Abs "INC", Undoc
  , Rel "BEQ", Iny "SBC", Undoc    , Undoc
  , Undoc    , Zpx "SBC", Zpx "INC", Undoc
  , Imp "SED", Aby "SBC", Undoc    , Undoc
  , Undoc    , Abx "SBC", Abx "INC", Undoc
  ]
