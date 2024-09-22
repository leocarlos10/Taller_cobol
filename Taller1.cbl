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
       ASSIGN TO "datos.txt"
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
           05 Muestra-salario-basico PIC 9(7).
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
      * variables para poder buscar un empleado por el nombre
        77 Nombre-Buscado PIC x(30).
        77 encontrado PIC x VALUE "N".
      * variables para la funcionalidad de empleado que mas
        77 SALARIO-MAX PIC x(30).
        77 NOMBRE-EMPLEADO PIC x(30).
      * promedio del salario basico de los empleados.
        77 contador PIC 99.
        77 suma-salarios PIC 9(8).
        77 promedio-salario-basico PIC 9(8).
        77 band-Empleados-encontrados PIC x VALUE "N".



       PROCEDURE DIVISION.

       MAIN-LOGIC SECTION.

       Empezar-programa.
           PERFORM INTERFAZ-APP.

           IF OPCION = 0
               STOP RUN
           END-IF

           IF OPCION = 1
            PERFORM Apertura-archivo
            MOVE ZEROES TO Maximos-Registros
            MOVE "1" TO Fin-Del-Archivo
            PERFORM Lee-Siguiente-Registro
            PERFORM Muestra-Registro
               UNTIL Fin-Del-Archivo = "0"
            PERFORM cerrar-registro

           END-IF

           IF OPCION = 2
            PERFORM Abrir-archivo
            MOVE "S" TO si-no
            PERFORM Agregar-registro
             UNTIL si-no = "N"
           PERFORM cerrar-registro

           END-IF

           IF OPCION = 3
              PERFORM EMPLEADO-MAS-GANA
           END-IF

           IF OPCION = 4
             PERFORM Buscar-Empleado-Por-Nombre
           END-IF

           IF OPCION = 5
               PERFORM MostrarEmpl-sal-encima-promedio
           END-IF

           GO TO Empezar-programa.



       Interfaz-App.
           DISPLAY "-----------------------------------".
           DISPLAY " Bienvenido Al Sistemas Empleados  ".
           DISPLAY "-----------------------------------".

           DISPLAY "1. Mostrar todos los empleados".
           DISPLAY "2. Ingresar empleados".
           DISPLAY "3. Mostrar el empleado que mas gana ".
           DISPLAY "4. Buscar info empleado por el nombre".
           DISPLAY
           "5. Mostrar empleados con salarios por encima del promedio".

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
           IF Maximos-Registros = 5
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


      * codigo para buscar empleado por el nombre

       Buscar-Empleado-Por-Nombre.
           DISPLAY "Ingrese el nombre del empleado que desea buscar".
           ACCEPT Nombre-Buscado.
           MOVE "1" TO Fin-Del-Archivo.
           PERFORM Apertura-archivo.
           PERFORM UNTIL FIN-DEL-ARCHIVO = "0" OR encontrado = "S"
               READ EMPLEADO-ARCHIVO NEXT RECORD
                   AT END
                       *>Cambia a "0" cuando se llega al final del archivo
                       MOVE "0" TO FIN-DEL-ARCHIVO
                   NOT AT END
                       If Empleado-nombre = Nombre-Buscado
                           MOVE "S" TO encontrado
                           DISPLAY "Empleado encontrado"
                           PERFORM Muestra-empleado
               END-READ
           END-PERFORM
           PERFORM cerrar-registro.
           IF encontrado = "N"
               DISPLAY "El empleado no se encuentra en el archivo."
           END-IF.

       Muestra-empleado.
           MOVE Empleado-cedula TO Muestra-cedula.
           MOVE Empleado-nombre TO Muestra-nombre.
           MOVE Empleado-direccion TO Muestra-direccion.
           MOVE Empleado-telefono TO Muestra-telefono.
           MOVE Empleado-salario-basico TO Muestra-salario-basico.
           DISPLAY PRESENTACION.


       EMPLEADO-MAS-GANA.

           OPEN INPUT  EMPLEADO-ARCHIVO.
           MOVE ZEROES TO MAXIMOS-REGISTROS.
           MOVE "1" TO Fin-Del-Archivo.

            *> Bucle que termina cuando se llega al final del archivo
           PERFORM UNTIL Fin-Del-Archivo = "0"
               READ EMPLEADO-ARCHIVO NEXT RECORD
                   AT END
                       *>Cambia a "0" cuando se llega al final del archivo
                       MOVE "0" TO Fin-Del-Archivo
                   NOT AT END
                       IF EMPLEADO-SALARIO-BASICO > SALARIO-MAX
                           MOVE EMPLEADO-NOMBRE TO NOMBRE-EMPLEADO
                           MOVE EMPLEADO-SALARIO-BASICO TO SALARIO-MAX
               END-READ
           END-PERFORM

           DISPLAY "Empleado con el salario maximo: "
           DISPLAY " " EMPLEADO-NOMBRE.
           DISPLAY " " SALARIO-MAX.

           CLOSE EMPLEADO-ARCHIVO.

      * codigo para buscar los empleado que tienen sueldos por encima
      * del promedio.

       MostrarEmpl-sal-encima-promedio.
           PERFORM Calcular-promedio-salarios.
           MOVE "1" TO Fin-Del-Archivo.
           PERFORM Apertura-archivo.
           PERFORM UNTIL Fin-Del-Archivo = "0"
               READ EMPLEADO-ARCHIVO NEXT RECORD
                   AT END
                       MOVE "0" TO Fin-Del-Archivo
                   NOT AT END
                       IF Empleado-salario-basico >
                           promedio-salario-basico
                           MOVE "S" TO band-Empleados-encontrados
                           DISPLAY
                          "Empleado con salario por encima del promedio"
                           PERFORM Muestra-empleado
                END-READ
           END-PERFORM
           PERFORM cerrar-registro.
           IF band-Empleados-encontrados = "N"
               DISPLAY "No se encontro ningun empleado"
               DISPLAY "Con salario mayor al promedio"
           END-IF.

       Calcular-promedio-salarios.
           MOVE ZEROES TO suma-salarios.
           MOVE ZEROES TO contador.
           MOVE ZEROES TO promedio-salario-basico.
           MOVE "1" TO Fin-Del-Archivo.
           PERFORM Apertura-archivo.
           PERFORM UNTIL Fin-Del-Archivo = "0"
               READ EMPLEADO-ARCHIVO NEXT RECORD
                   AT END
                       *>Cambia a "0" cuando se llega al final del archivo
                       MOVE "0" TO Fin-Del-Archivo
                   NOT AT END
                       COMPUTE
                           suma-salarios =
                           suma-salarios + Empleado-salario-basico
                           ADD 1 TO contador
               END-READ
           END-PERFORM
           COMPUTE promedio-salario-basico = suma-salarios/contador.
           PERFORM cerrar-registro.

       END PROGRAM Taller1.
