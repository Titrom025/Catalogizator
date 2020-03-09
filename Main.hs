import System.Directory
import MenuSource
import FileHandler
import System.IO
import System.Console.ANSI

main = do
    getFilesInfo

    setSGR [SetColor Foreground Vivid Yellow]
    clearScreen 
    print_menu

    io_handler
