-- file ch17/PCRE-compile0.hs

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Regex

type PCRE = ()

foreign import ccall unsafe "pcre.h pcre_compile"
    c_pcre_compile  :: CString
                    -> PCREOption
                    -> Ptr CString
                    -> Ptr CInt
                    -> Ptr Word8
                    -> IO (Ptr PCRE)

