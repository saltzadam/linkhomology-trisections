module Classification.Testing.SSSTest
  where
import Classification.Tester
import Core.Configuration (Configuration)

sss1 :: Configuration
sss1 = fromPic sss1string
sss1string :: [Char]
sss1string =  ".-.\n\
               \| |\n\
               \. .\n\
               \|>|\n\
               \. .\n\
               \|>|\n\
               \. .\n\
               \|>|\n\
               \.-."

sss2 :: Configuration
sss2 = fromPic sss2string
sss2string :: [Char]
sss2string =   ".-.\n\
               \| |\n\
               \.-.\n\
               \ v \n\
               \.-.\n\
               \| |\n\
               \.-.\n\
               \ v \n\
               \.-.\n\
               \| |\n\
               \.-.\n\
               \ ^ \n\
               \.-.\n\
               \| |\n\
               \.-."

sss3 :: Configuration
sss3 = fromPic $ sss1string ++ "\n\n" ++ sss2string
