module Network.EasyBitcoin.Internal.Script 
 where



import Data.Word
import qualified Data.ByteString as BS
import Data.List (nub) -- not to use nub!! TODO


import Data.Binary (Binary, get, put)
import Data.Binary.Get ( getWord64be
                       , getWord32be
                       , getWord8
                       , getWord16le
                       , getWord32le
                       , getByteString
                       , Get
                       , isEmpty    
                       )
import Data.Binary.Put( putWord64be
                      , putWord32be
                      , putWord32le
                      , putWord16le
                      , putWord8
                      , putByteString
                      )
import Control.Monad                   (unless, guard,replicateM,forM_,liftM2)
import Control.Applicative((<$>))


class ScriptType script where
    generalScript :: script -> Script
    interpret     :: Script -> Maybe script


newtype Script = Script{scriptOps::[ScriptOp]} 
                 deriving (Eq, Show, Read)


-- | Data type representing all of the operators allowed inside a 'Script'.
data ScriptOp   = OP_PUSHDATA { pushContent :: !BS.ByteString
                              , pushOpCode  :: !PushDataType
                              }
              --   OP__ 0
                
                | OP_1NEGATE
                | OP_RESERVED
                | OP__ Word8 -- from 0 to 16 inclusive
                
                -- ^ Flow control
                | OP_NOP
                | OP_VER -- reserved
                | OP_IF
                | OP_NOTIF
                | OP_VERIF -- resreved
                | OP_VERNOTIF -- reserved
                | OP_ELSE
                | OP_ENDIF
                | OP_VERIFY
                | OP_RETURN
                
                -- ^Stack operations
                | OP_TOALTSTACK
                | OP_FROMALTSTACK
                | OP_IFDUP
                | OP_DEPTH
                | OP_DROP
                | OP_DUP
                | OP_NIP
                | OP_OVER
                | OP_PICK
                | OP_ROLL
                | OP_ROT
                | OP_SWAP
                | OP_TUCK
                | OP_2DROP
                | OP_2DUP
                | OP_3DUP
                | OP_2OVER
                | OP_2ROT
                | OP_2SWAP
                
                -- ^ Splice
                | OP_CAT
                | OP_SUBSTR
                | OP_LEFT
                | OP_RIGHT
                | OP_SIZE
                
                -- ^ Bitwise logic
                | OP_INVERT
                | OP_AND
                | OP_OR
                | OP_XOR
                | OP_EQUAL
                | OP_EQUALVERIFY
                | OP_RESERVED1
                | OP_RESERVED2
                
                -- ^ Arithmetic
                | OP_1ADD
                | OP_1SUB
                | OP_2MUL
                | OP_2DIV
                | OP_NEGATE
                | OP_ABS
                | OP_NOT
                | OP_0NOTEQUAL
                | OP_ADD
                | OP_SUB
                | OP_MUL
                | OP_DIV
                | OP_MOD
                | OP_LSHIFT
                | OP_RSHIFT
                | OP_BOOLAND
                | OP_BOOLOR
                | OP_NUMEQUAL
                | OP_NUMEQUALVERIFY
                | OP_NUMNOTEQUAL
                | OP_LESSTHAN
                | OP_GREATERTHAN
                | OP_LESSTHANOREQUAL
                | OP_GREATERTHANOREQUAL
                | OP_MIN
                | OP_MAX
                | OP_WITHIN
                
                -- ^ Crypto
                | OP_RIPEMD160
                | OP_SHA1
                | OP_SHA256
                | OP_HASH160
                | OP_HASH256
                | OP_CODESEPARATOR
                | OP_CHECKSIG
                | OP_CHECKSIGVERIFY
                | OP_CHECKMULTISIG
                | OP_CHECKMULTISIGVERIFY
                
                -- ^ Expansion
                | OP_NOP1 | OP_NOP2 | OP_NOP3 | OP_NOP4 | OP_NOP5
                | OP_NOP6 | OP_NOP7 | OP_NOP8 | OP_NOP9 | OP_NOP10
                
                -- ^ Other
                | OP_PUBKEYHASH
                | OP_PUBKEY
                | OP_INVALIDOPCODE  !Word8 
                deriving (Show, Read, Eq)



-- | Data type representing the type of an OP_PUSHDATA opcode.
data PushDataType = OPCODE      -- | The next opcode bytes is data to be pushed onto the stack

                                -- | The next byte contains the number of bytes to be pushed onto
                                -- the stack
                  | OPDATA1
                 
                                -- | The next two bytes contains the number of bytes to be pushed onto
                                -- the stack
                  | OPDATA2
                                -- | The next four bytes contains the number of bytes to be pushed onto
                                -- the stack
                  | OPDATA4
                  deriving (Show, Read, Eq)





----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------

op:: Int -> ScriptOp
op n = let n_   = min 16 (max 0 n) 
        in OP__ (fromIntegral n) 



opPushData :: BS.ByteString -> ScriptOp
opPushData bs = case BS.length bs of 
                  len | len <= 0x4b       -> OP_PUSHDATA bs OPCODE
                      | len <= 0xff       -> OP_PUSHDATA bs OPDATA1
                      | len <= 0xffff     -> OP_PUSHDATA bs OPDATA2
                      | len <= 0xffffffff -> OP_PUSHDATA bs OPDATA4
                      | otherwise         -> error "opPushData: payload size too big"




opNumber :: ScriptOp -> Maybe Int
opNumber (OP__ n) = Just (fromIntegral n)
opNumber _        = Nothing 

opContent :: ScriptOp -> Maybe BS.ByteString
opContent (OP_PUSHDATA content _) = Just content
opContent _                       = Nothing 


---------------------------------------------------------------------------------------------------

instance Binary Script where
   
   get = Script <$> getScriptOps
    where 
      getScriptOps = do empty <- isEmpty
                        if empty
                          then return []
                          else liftM2 (:) get getScriptOps
   
   put (Script ops) = forM_ ops put




instance Binary ScriptOp where
        get = do op <- getWord8
                 case op of
                  0x00           -> return $ OP__ 0

                  _ | op <= 0x4b -> do payload <- getByteString (fromIntegral op)
                                       return $ OP_PUSHDATA payload OPCODE
                  
                  0x4c           -> do len     <- getWord8
                                       payload <- getByteString (fromIntegral len)
                                       return   $ OP_PUSHDATA payload OPDATA1
                  
                  0x4d           -> do len     <- getWord16le
                                       payload <- getByteString (fromIntegral len)
                                       return   $ OP_PUSHDATA payload OPDATA2
                  
                  0x4e           -> do len     <- getWord32le
                                       payload <- getByteString (fromIntegral len)
                                       return   $ OP_PUSHDATA payload OPDATA4


                  0x4f           -> return $ OP_1NEGATE
                  0x50           -> return $ OP_RESERVED
                  _ | op < 0x61  -> return $ OP__ (op - 0x50)
                -- Flow control
                  0x61           -> return $ OP_NOP
                  0x62           -> return $ OP_VER -- reserved
                  0x63           -> return $ OP_IF
                  0x64           -> return $ OP_NOTIF
                  0x65           -> return $ OP_VERIF -- reserved
                  0x66           -> return $ OP_VERNOTIF -- reserved
                  0x67           -> return $ OP_ELSE
                  0x68           -> return $ OP_ENDIF
                  0x69           -> return $ OP_VERIFY
                  0x6a           -> return $ OP_RETURN
                -- Stack
                  0x6b           -> return $ OP_TOALTSTACK
                  0x6c           -> return $ OP_FROMALTSTACK
                  0x6d           -> return $ OP_2DROP
                  0x6e           -> return $ OP_2DUP
                  0x6f           -> return $ OP_3DUP
                  0x70           -> return $ OP_2OVER
                  0x71           -> return $ OP_2ROT
                  0x72           -> return $ OP_2SWAP
                  0x73           -> return $ OP_IFDUP
                  0x74           -> return $ OP_DEPTH
                  0x75           -> return $ OP_DROP
                  0x76           -> return $ OP_DUP
                  0x77           -> return $ OP_NIP
                  0x78           -> return $ OP_OVER
                  0x79           -> return $ OP_PICK
                  0x7a           -> return $ OP_ROLL
                  0x7b           -> return $ OP_ROT
                  0x7c           -> return $ OP_SWAP
                  0x7d           -> return $ OP_TUCK
                -- Splice
                  0x7e           -> return $ OP_CAT
                  0x7f           -> return $ OP_SUBSTR
                  0x80           -> return $ OP_LEFT
                  0x81           -> return $ OP_RIGHT
                  0x82           -> return $ OP_SIZE
                -- Bitwise logic
                  0x83           -> return $ OP_INVERT
                  0x84           -> return $ OP_AND
                  0x85           -> return $ OP_OR
                  0x86           -> return $ OP_XOR
                  0x87           -> return $ OP_EQUAL
                  0x88           -> return $ OP_EQUALVERIFY
                  0x89           -> return $ OP_RESERVED1
                  0x8a           -> return $ OP_RESERVED2
                -- Arithmetic
                  0x8b           -> return $ OP_1ADD
                  0x8c           -> return $ OP_1SUB
                  0x8d           -> return $ OP_2MUL
                  0x8e           -> return $ OP_2DIV
                  0x8f           -> return $ OP_NEGATE
                  0x90           -> return $ OP_ABS
                  0x91           -> return $ OP_NOT
                  0x92           -> return $ OP_0NOTEQUAL
                  0x93           -> return $ OP_ADD
                  0x94           -> return $ OP_SUB
                  0x95           -> return $ OP_MUL
                  0x96           -> return $ OP_DIV
                  0x97           -> return $ OP_MOD
                  0x98           -> return $ OP_LSHIFT
                  0x99           -> return $ OP_RSHIFT
                  0x9a           -> return $ OP_BOOLAND
                  0x9b           -> return $ OP_BOOLOR
                  0x9c           -> return $ OP_NUMEQUAL
                  0x9d           -> return $ OP_NUMEQUALVERIFY
                  0x9e           -> return $ OP_NUMNOTEQUAL
                  0x9f           -> return $ OP_LESSTHAN
                  0xa0           -> return $ OP_GREATERTHAN
                  0xa1           -> return $ OP_LESSTHANOREQUAL
                  0xa2           -> return $ OP_GREATERTHANOREQUAL
                  0xa3           -> return $ OP_MIN
                  0xa4           -> return $ OP_MAX
                  0xa5           -> return $ OP_WITHIN
                -- Crypto
                  0xa6           -> return $ OP_RIPEMD160
                  0xa7           -> return $ OP_SHA1
                  0xa8           -> return $ OP_SHA256
                  0xa9           -> return $ OP_HASH160
                  0xaa           -> return $ OP_HASH256
                  0xab           -> return $ OP_CODESEPARATOR
                  0xac           -> return $ OP_CHECKSIG
                  0xad           -> return $ OP_CHECKSIGVERIFY
                  0xae           -> return $ OP_CHECKMULTISIG
                  0xaf           -> return $ OP_CHECKMULTISIGVERIFY
                -- More NOPs
                  0xb0           -> return $ OP_NOP1
                  0xb1           -> return $ OP_NOP2
                  0xb2           -> return $ OP_NOP3
                  0xb3           -> return $ OP_NOP4
                  0xb4           -> return $ OP_NOP5
                  0xb5           -> return $ OP_NOP6
                  0xb6           -> return $ OP_NOP7
                  0xb7           -> return $ OP_NOP8
                  0xb8           -> return $ OP_NOP9
                  0xb9           -> return $ OP_NOP10
                -- Constants
                  0xfd           -> return $ OP_PUBKEYHASH
                  0xfe           -> return $ OP_PUBKEY
                  _              -> return $ OP_INVALIDOPCODE op

        put op = case op of
                (OP_PUSHDATA payload optype)-> do let len = BS.length payload
                                                  case optype of
                                                    OPCODE -> do unless (len <= 0x4b) $ fail        "OP_PUSHDATA OPCODE: Payload size too big"
                                                                 putWord8 $ fromIntegral len
                
                                                    OPDATA1 -> do unless (len <= 0xff) $ fail       "OP_PUSHDATA OPDATA1: Payload size too big"
                                                                  putWord8 0x4c
                                                                  putWord8 $ fromIntegral len
                                                    
                                                    OPDATA2 -> do unless (len <= 0xffff) $ fail     "OP_PUSHDATA OPDATA2: Payload size too big"
                                                                  putWord8 0x4d
                                                                  putWord16le $ fromIntegral len
                                                    
                                                    OPDATA4 -> do unless (len <= 0x7fffffff) $ fail "OP_PUSHDATA OPDATA4: Payload size too big"
                                                                  putWord8 0x4e
                                                                  putWord32le $ fromIntegral len
                                                  putByteString payload
                -- Constants
                OP__ 0      -> putWord8 0x00
                OP_1NEGATE  -> putWord8 0x4f
                OP_RESERVED -> putWord8 0x50
                OP__ n -> putWord8 (0x50+n) -- n should be between 1 and 16 inclusive
                -- Crypto Constants
                OP_PUBKEY -> putWord8 0xfe
                OP_PUBKEYHASH -> putWord8 0xfd
                -- Invalid Opcodes
                (OP_INVALIDOPCODE x) -> putWord8 x
                -- Flow Control
                OP_NOP -> putWord8 0x61
                OP_VER -> putWord8 0x62
                OP_IF -> putWord8 0x63
                OP_NOTIF -> putWord8 0x64
                OP_VERIF -> putWord8 0x65
                OP_VERNOTIF -> putWord8 0x66
                OP_ELSE -> putWord8 0x67
                OP_ENDIF -> putWord8 0x68
                OP_VERIFY -> putWord8 0x69
                OP_RETURN -> putWord8 0x6a
                -- Stack Operations
                OP_TOALTSTACK -> putWord8 0x6b
                OP_FROMALTSTACK -> putWord8 0x6c
                OP_2DROP -> putWord8 0x6d
                OP_2DUP -> putWord8 0x6e
                OP_3DUP -> putWord8 0x6f
                OP_2OVER -> putWord8 0x70
                OP_2ROT -> putWord8 0x71
                OP_2SWAP -> putWord8 0x72
                OP_IFDUP -> putWord8 0x73
                OP_DEPTH -> putWord8 0x74
                OP_DROP -> putWord8 0x75
                OP_DUP -> putWord8 0x76
                OP_NIP -> putWord8 0x77
                OP_OVER -> putWord8 0x78
                OP_PICK -> putWord8 0x79
                OP_ROLL -> putWord8 0x7a
                OP_ROT -> putWord8 0x7b
                OP_SWAP -> putWord8 0x7c
                OP_TUCK -> putWord8 0x7d
                -- Splice
                OP_CAT -> putWord8 0x7e
                OP_SUBSTR -> putWord8 0x7f
                OP_LEFT -> putWord8 0x80
                OP_RIGHT -> putWord8 0x81
                OP_SIZE -> putWord8 0x82
                -- Bitwise Logic
                OP_INVERT -> putWord8 0x83
                OP_AND -> putWord8 0x84
                OP_OR -> putWord8 0x85
                OP_XOR -> putWord8 0x86
                OP_EQUAL -> putWord8 0x87
                OP_EQUALVERIFY -> putWord8 0x88
                OP_RESERVED1 -> putWord8 0x89
                OP_RESERVED2 -> putWord8 0x8a
                -- Arithmetic
                OP_1ADD -> putWord8 0x8b
                OP_1SUB -> putWord8 0x8c
                OP_2MUL -> putWord8 0x8d
                OP_2DIV -> putWord8 0x8e
                OP_NEGATE -> putWord8 0x8f
                OP_ABS -> putWord8 0x90
                OP_NOT -> putWord8 0x91
                OP_0NOTEQUAL -> putWord8 0x92
                OP_ADD -> putWord8 0x93
                OP_SUB -> putWord8 0x94
                OP_MUL -> putWord8 0x95
                OP_DIV -> putWord8 0x96
                OP_MOD -> putWord8 0x97
                OP_LSHIFT -> putWord8 0x98
                OP_RSHIFT -> putWord8 0x99
                OP_BOOLAND -> putWord8 0x9a
                OP_BOOLOR -> putWord8 0x9b
                OP_NUMEQUAL -> putWord8 0x9c
                OP_NUMEQUALVERIFY -> putWord8 0x9d
                OP_NUMNOTEQUAL -> putWord8 0x9e
                OP_LESSTHAN -> putWord8 0x9f
                OP_GREATERTHAN -> putWord8 0xa0
                OP_LESSTHANOREQUAL -> putWord8 0xa1
                OP_GREATERTHANOREQUAL-> putWord8 0xa2
                OP_MIN -> putWord8 0xa3
                OP_MAX -> putWord8 0xa4
                OP_WITHIN -> putWord8 0xa5
                -- Crypto
                OP_RIPEMD160 -> putWord8 0xa6
                OP_SHA1 -> putWord8 0xa7
                OP_SHA256 -> putWord8 0xa8
                OP_HASH160 -> putWord8 0xa9
                OP_HASH256 -> putWord8 0xaa
                OP_CODESEPARATOR -> putWord8 0xab
                OP_CHECKSIG -> putWord8 0xac
                OP_CHECKSIGVERIFY -> putWord8 0xad
                OP_CHECKMULTISIG -> putWord8 0xae
                OP_CHECKMULTISIGVERIFY -> putWord8 0xaf
                -- More NOPs
                OP_NOP1 -> putWord8 0xb0
                OP_NOP2 -> putWord8 0xb1
                OP_NOP3 -> putWord8 0xb2
                OP_NOP4 -> putWord8 0xb3
                OP_NOP5 -> putWord8 0xb4
                OP_NOP6 -> putWord8 0xb5
                OP_NOP7 -> putWord8 0xb6
                OP_NOP8 -> putWord8 0xb7
                OP_NOP9 -> putWord8 0xb8
                OP_NOP10 -> putWord8 0xb9


































