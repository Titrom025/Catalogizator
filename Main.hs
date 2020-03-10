import MenuSource
import FileHandler
import System.Console.ANSI (SGR(SetColor), setSGR, clearScreen, 
    ConsoleLayer(Foreground), ColorIntensity(Vivid), Color(Yellow))

main :: IO ()
main = do
    getFilesInfo
    setSGR [SetColor Foreground Vivid Yellow]
    clearScreen 
    print_menu
    io_handler
