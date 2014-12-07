@echo off

echo Iris K = 2
C:\Python33\python.exe C:\Study\HSE\Data_Analisys\data_analisys\build_algorithm\build_algorithm.py --csv ..\InputIris\test1.csv --K 2
C:\Python33\python.exe C:\Study\HSE\Data_Analisys\data_analisys\build_algorithm\build_algorithm.py --csv ..\InputIris\test2.csv --K 2
C:\Python33\python.exe C:\Study\HSE\Data_Analisys\data_analisys\build_algorithm\build_algorithm.py --csv ..\InputIris\test3.csv --K 2
C:\Python33\python.exe C:\Study\HSE\Data_Analisys\data_analisys\build_algorithm\build_algorithm.py --csv ..\InputIris\test4.csv --K 2

echo Iris K = 3
C:\Python33\python.exe C:\Study\HSE\Data_Analisys\data_analisys\build_algorithm\build_algorithm.py --csv ..\InputIris\test1.csv --K 3 --check_error ..\InputIris\sourcedata.csv
C:\Python33\python.exe C:\Study\HSE\Data_Analisys\data_analisys\build_algorithm\build_algorithm.py --csv ..\InputIris\test2.csv --K 3 --check_error ..\InputIris\sourcedata.csv
C:\Python33\python.exe C:\Study\HSE\Data_Analisys\data_analisys\build_algorithm\build_algorithm.py --csv ..\InputIris\test3.csv --K 3 --check_error ..\InputIris\sourcedata.csv
C:\Python33\python.exe C:\Study\HSE\Data_Analisys\data_analisys\build_algorithm\build_algorithm.py --csv ..\InputIris\test4.csv --K 3 --check_error ..\InputIris\sourcedata.csv

echo Iris K = 4
C:\Python33\python.exe C:\Study\HSE\Data_Analisys\data_analisys\build_algorithm\build_algorithm.py --csv ..\InputIris\test1.csv --K 4
C:\Python33\python.exe C:\Study\HSE\Data_Analisys\data_analisys\build_algorithm\build_algorithm.py --csv ..\InputIris\test2.csv --K 4
C:\Python33\python.exe C:\Study\HSE\Data_Analisys\data_analisys\build_algorithm\build_algorithm.py --csv ..\InputIris\test3.csv --K 4
C:\Python33\python.exe C:\Study\HSE\Data_Analisys\data_analisys\build_algorithm\build_algorithm.py --csv ..\InputIris\test4.csv --K 4