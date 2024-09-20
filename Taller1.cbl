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
       SELECT OPTIONAL EMPLEADO-ARCHIVO
           ASSIGN TO
           "Taller1.txt"
           ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD EMPLEADO-ARCHIVO.
         01 Empleados-registro.
               05 Empleado-cedula PIC x(11).
               05 Empleado-nombre PIC x(30).
               05 Empleado-direccion PIC x(20).
               05 Empleado-telefono PIC x(12).
               05 Empleado-salario-basico PIC 9(5)v999999.

       WORKING-STORAGE SECTION.

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
               DISPLAY "Mostrando Empleados".
           IF OPCION = 2
               PERFORM Abrir-archivo
               MOVE "S" TO si-no
               PERFORM Agregar-registro
                   UNTIL si-no = "N"
               PERFORM cerrar-registro.
           
           IF OPCION = 3
              DISPLAY "Mostrar Empleado que mas gana".
           
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