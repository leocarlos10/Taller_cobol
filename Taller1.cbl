      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Taller1.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
       SELECT OPTIONAL  EMPLEADO-ARCHIVO
       ASSIGN TO
       "datos.txt"
       ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD EMPLEADO-ARCHIVO.
         01 Empleados-registro.
               05 Empleado-cedula PIC x(11).
               05 Empleado-nombre PIC x(30).
               05 Empleado-direccion PIC x(20).
               05 Empleado-telefono PIC x(12).
               05 Empleado-salario-basico PIC 9(8).

       WORKING-STORAGE SECTION.
      * variables para poder mostrar los empleados.
       01 Presentacion.
           05 Texto-cedula PIC x(5) VALUE "ID: ".
           05 Muestra-cedula PIC x(17).
           05 Texto-nombre PIC x(10) VALUE "Nombre: ".
           05 Muestra-nombre PIC x(10).
           05 Texto-direccion PIC x(11) VALUE "direccion: ".
           05 Muestra-direccion PIC x(10).
           05 Texto-telefono PIC x(10) VALUE "telefono: ".
           05 Muestra-telefono PIC x(10).
           05 Texto-salario-basico PIC x(19) VALUE "salario basico: ".
           05 Muestra-salario-basico PIC x(15).
           01  Fin-Del-Archivo PIC X.
           01  Maximos-Registros PIC 99.
           01  Guarda-Enter PIC X.
      * variables para guardar los datos de los empleados.
         77 cedula PIC x(17)    VALUE  "Ingresa tu cedula".
         77 nombre PIC x(17)    VALUE  "Ingresa tu nombre".
         77 direccion PIC x(22) VALUE  "Ingresa tu direccion".
         77 telefono PIC x(20)  VALUE  "Ingresa tu telefono".
         77 salario-basico PIC x(25) VALUE "Ingresa tu salario basico".
         77 si-no PIC x.
         77 entrada PIC x.
         77 opcion PIC x.



       PROCEDURE DIVISION.

       MAIN-LOGIC SECTION.

       Empezar-programa.
           PERFORM INTERFAZ-APP.

           IF OPCION = 1
               PERFORM  Apertura-archivo.
               MOVE ZEROES TO Maximos-Registros.
               MOVE "1" TO Fin-Del-Archivo.
               PERFORM Lee-Siguiente-Registro.
               PERFORM  Muestra-Registro
               UNTIL Fin-Del-Archivo = "0".
               PERFORM cerrar-registro.
               STOP RUN.
           IF OPCION = 2
               PERFORM Abrir-archivo
               MOVE "S" TO si-no
               PERFORM Agregar-registro
                   UNTIL si-no = "N"
               PERFORM cerrar-registro.

           IF OPCION = 3
               PERFORM Apertura-archivo.


           IF OPCION = 4
              DISPLAY "Total de La nomina".

           IF OPCION = 5
              DISPLAY "Promedio Sueldos Basicos".

           STOP RUN.


       Interfaz-App.
           DISPLAY "-----------------------------------".
           DISPLAY " Bienvenido Al Sistemas Empleados  ".
           DISPLAY "-----------------------------------".

           DISPLAY "1. Mostrar todos los empleados".
           DISPLAY "2. Ingresar empleados".
           DISPLAY "3. Mostrar empleado Que mas gana".
           DISPLAY "4. Total de la nomina".
           DISPLAY "5. Calcular Promedio Sueldos Basicos".

           DISPLAY "0. Cerrar".


           DISPLAY "Selecione > ".
           ACCEPT  opcion.

      * codigo para mostrar todos los empleados
       Apertura-archivo.
       OPEN INPUT EMPLEADO-ARCHIVO.

        Muestra-Registro.
           PERFORM Muestra-campos.
           PERFORM Lee-Siguiente-Registro.


        Muestra-campos.
           IF Maximos-Registros = 10
           PERFORM PULSAR-ENTER.
           MOVE Empleado-cedula TO Muestra-cedula.
           MOVE Empleado-nombre TO Muestra-nombre.
           MOVE Empleado-direccion TO Muestra-direccion.
           MOVE Empleado-telefono TO Muestra-telefono.
           MOVE Empleado-salario-basico TO Muestra-salario-basico.
           DISPLAY PRESENTACION.
           ADD 1 TO Maximos-Registros.


       Lee-Siguiente-Registro.
           READ EMPLEADO-ARCHIVO NEXT RECORD
           AT END
           MOVE "0" TO FIN-DEL-ARCHIVO.

       PULSAR-ENTER.
           DISPLAY
           "Presione la tecla ENTER para ver la siguiente pagina...".
           ACCEPT Guarda-Enter.
           MOVE ZEROES TO Maximos-Registros.

      * codigo para registrar Empleados
       Abrir-archivo.
           OPEN EXTEND EMPLEADO-ARCHIVO.

       cerrar-registro.
           CLOSE EMPLEADO-ARCHIVO.


       Agregar-registro.
           MOVE "N" TO entrada.
           PERFORM Obtener-campos
           UNTIL entrada = "S".
           PERFORM Escribir-registro.
           PERFORM Reiniciar.


       Escribir-registro.
           WRITE Empleados-registro.

       Obtener-campos.
           MOVE SPACE TO Empleados-registro.
           DISPLAY cedula.
           ACCEPT Empleado-cedula.
           DISPLAY nombre.
           ACCEPT Empleado-nombre.
           DISPLAY direccion.
           ACCEPT Empleado-direccion.
           DISPLAY telefono.
           ACCEPT Empleado-telefono.
           DISPLAY salario-basico.
           ACCEPT Empleado-salario-basico.
           PERFORM Continuar.

       Continuar.
           MOVE "S" TO entrada.

           if Empleado-nombre = SPACE
               MOVE "N" TO entrada.

       Reiniciar.
           DISPLAY "Desea ingresar otro registro (S/N) ?".
           ACCEPT SI-NO.

           IF SI-NO = "s"
               MOVE "S" TO SI-NO.

           IF SI-NO NOT = "S"
               MOVE "N" TO SI-NO.


       END PROGRAM Taller1.
