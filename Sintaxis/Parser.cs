using AnalizadorLexico.Lexico;
using Avalonia.Controls;
using System;
using System.Collections.Generic;
using System.Linq;


namespace AnalizadorLexico.Sintaxis
{
    /// <summary>
    /// Clase que implementa un parser LL(1) para el subconjunto de FORTRAN77.
    /// Esta clase toma una lista de tokens y construye un árbol sintáctico.
    /// </summary>
    public class Parser
    {
        private readonly List<Token> _tokens;
        private int _posicionActual;
        private bool EsFin() => _posicionActual >= _tokens.Count || Actual().Tipo == TipoToken.EOF;
        private Token Actual() => _tokens[_posicionActual];
        private Token Anterior() => _tokens[_posicionActual - 1];


        public Parser(IEnumerable<Token> tokens)
        {
            _tokens = tokens.ToList();
        }

        public Programa ParsePrograma()
        {
            var prog = new Programa();
            while (!Check(TipoToken.EOF))
            {
                var sentencia = ParseSentencia();
                if (sentencia != null)
                    prog.Sentencias.Add(sentencia);
            }
            return prog;
        }


        /// <summary>
        ///    Aplicar un parser descendente para una sentencia.
        /// </summary>
        /// <returns>Devuelve un nodo sintáctico que representa la sentencia.</returns>
        private NodoSintactico? ParseSentencia()
        {
            /// verificar si es una asignación
            if (Check(TipoToken.Identificador))
            {
                var id = Consumir(TipoToken.Identificador, "Se esperaba identificador");
                Consumir(TipoToken.Asignacion, "Se esperaba '='");
                var expr = ParseExpresion();
                return new Asignacion(id.Lexema, expr);
            }
            /// verificar si es una estructura de control
            else if (Check(TipoToken.PalabraReservada))
            {
                var palabra = Actual().Lexema;
                if (palabra == "IF") return ParseIf();
                if (palabra == "DO") return ParseDo();
                if (palabra == "INTEGER" || palabra == "REAL") return ParseDeclaracion();
                throw new Exception($"Palabra reservada no soportada: {palabra}");
            }
            /// verificar si es una sentencia vacía
            else if (Check(TipoToken.EOL))
            {
                Avanzar(); // ignorar fin de línea
                return ParseSentencia();
            }
            /// verificar si es el final del archivo
            else if (Check(TipoToken.EOF))
            {
                return null;
            }
            else
            {
                throw new Exception($"Token inesperado: {Actual()}");
            }
        }



        /// <summary>
        /// Aplicar un parser descendente para una expresión.
        /// </summary>
        /// <returns>Devuelve un nodo sintáctico que representa la expresión.</returns>
        private Expresion ParseExpresion()
        {
            var izq = ParseTermino();
            while (Check(TipoToken.OperadorAritmetico) &&
                   (Actual().Lexema == "+" || Actual().Lexema == "-"))
            {
                var op = Avanzar().Lexema;
                var der = ParseTermino();
                izq = new Binaria(op, izq, der);
            }
            return izq;
        }

        /// <summary>
        ///     Aplicar un parser descendente para un término.
        /// </summary>
        /// <returns>Devuelve un nodo sintáctico que representa el término.</returns>
        private Expresion ParseTermino()
        {
            var izq = ParseFactor();
            while (Check(TipoToken.OperadorAritmetico) &&
                   (Actual().Lexema == "*" || Actual().Lexema == "/"))
            {
                var op = Avanzar().Lexema;
                var der = ParseFactor();
                izq = new Binaria(op, izq, der);
            }
            return izq;
        }


        /// <summary>
        ///     Aplicar un parser descendente para un factor.
        /// </summary>
        /// <returns>Devuelve un nodo sintáctico que representa el factor.</returns>
        private Expresion ParseFactor()
        {
            if (Match(TipoToken.NumeroEntero) || Match(TipoToken.NumeroReal))
                return new Numero(Anterior().Lexema);

            if (Match(TipoToken.Identificador))
                return new Variable(Anterior().Lexema);

            if (Match(TipoToken.ParentesisApertura))
            {
                var expr = ParseExpresion();
                Consumir(TipoToken.ParentesisCierre, "Se esperaba ')'");
                return expr;
            }

            throw new Exception("Factor inválido");
        }

        /// <summary>
        ///   Aplicar un parser descendente para una estructura IF.
        /// </summary>
        /// <returns>Un nodo sintáctico que representa la estructura IF.</returns>
        private NodoSintactico ParseIf()
        {
            Consumir(TipoToken.PalabraReservada, "Se esperaba 'IF'");
            Consumir(TipoToken.ParentesisApertura, "Se esperaba '(' después de 'IF'");
            var condicion = ParseExpresion();
            Consumir(TipoToken.ParentesisCierre, "Se esperaba ')' después de la condición");
            Consumir(TipoToken.PalabraReservada, "Se esperaba 'THEN'");
            
            // Parsear bloque THEN
            var sentenciasThen = new List<NodoSintactico>();
            while (!Check(TipoToken.PalabraReservada) || 
                   (Actual().Lexema != "ELSE" && Actual().Lexema != "ENDIF"))
            {
                if (Check(TipoToken.EOF))
                    throw new Exception("Se esperaba 'ENDIF' o 'ELSE'");
                var sentencia = ParseSentencia();
                if (sentencia != null)
                    sentenciasThen.Add(sentencia);
            }
            
            // Parsear bloque ELSE si existe
            List<NodoSintactico>? sentenciasElse = null;
            if (Check(TipoToken.PalabraReservada) && Actual().Lexema == "ELSE")
            {
                Avanzar(); // consumir ELSE
                sentenciasElse = new List<NodoSintactico>();
                while (!Check(TipoToken.PalabraReservada) || Actual().Lexema != "ENDIF")
                {
                    if (Check(TipoToken.EOF))
                        throw new Exception("Se esperaba 'ENDIF'");
                    var sentencia = ParseSentencia();
                    if (sentencia != null)
                        sentenciasElse.Add(sentencia);
                }
            }
            
            // Consumir ENDIF
            Consumir(TipoToken.PalabraReservada, "Se esperaba 'ENDIF'");
            
            return new If(condicion, sentenciasThen, sentenciasElse);
        }

        /// <summary>
        ///  Aplicar un parser descendente para una estructura DO.
        ///  Sintaxis FORTRAN77: DO label variable = start, end [, increment]
        ///  Ejemplo: DO 100 I = 1, 5
        /// </summary>
        /// <returns>Un nodo sintáctico que representa la estructura DO.</returns>
        private NodoSintactico ParseDo()
        {
            Consumir(TipoToken.PalabraReservada, "Se esperaba 'DO'");
            
            // Leer el label (número)
            var labelToken = Consumir(TipoToken.NumeroEntero, "Se esperaba un label numérico después de 'DO'");
            var label = labelToken.Lexema;
            
            // Leer la variable del bucle
            var id = Consumir(TipoToken.Identificador, "Se esperaba identificador después del label");
            Consumir(TipoToken.Asignacion, "Se esperaba '=' después del identificador");
            
            // Leer el valor inicial
            var inicio = ParseExpresion();
            Consumir(TipoToken.Coma, "Se esperaba ',' después del valor inicial");
            
            // Leer el valor final
            var fin = ParseExpresion();
            
            // Leer el incremento opcional
            Expresion? incremento = null;
            if (Match(TipoToken.Coma))
            {
                incremento = ParseExpresion();
            }
            
            // Parsear las sentencias del bucle hasta encontrar el label con CONTINUE
            var sentencias = new List<NodoSintactico>();
            while (!Check(TipoToken.EOF))
            {
                // Verificar si encontramos el label seguido de CONTINUE
                if (Check(TipoToken.NumeroEntero) && Actual().Lexema == label)
                {
                    Avanzar(); // consumir el label
                    if (Check(TipoToken.PalabraReservada) && Actual().Lexema == "CONTINUE")
                    {
                        Avanzar(); // consumir CONTINUE
                        break;
                    }
                }
                var sentencia = ParseSentencia();
                if (sentencia != null)
                    sentencias.Add(sentencia);
            }
            
            return new DoLoop(label, id.Lexema, inicio, fin, sentencias, incremento);
        }

        /// <summary>
        ///  Aplicar un parser descendente para una declaración de variable.
        /// </summary>
        /// <returns>Un nodo sintáctico que representa la declaración de variable.</returns>
        private NodoSintactico ParseDeclaracion()
        {
            var tipo = Consumir(TipoToken.PalabraReservada, "Se esperaba tipo de dato");
            var id = Consumir(TipoToken.Identificador, "Se esperaba identificador en la declaración");
            return new Declaracion(tipo.Lexema, id.Lexema);
        }


        // utilidades


        /// <summary>
        ///   Consumir un token del tipo esperado o lanzar una excepción con el mensaje dado.
        /// </summary>
        /// <param name="tipo">Tipo de token esperado.</param>
        /// <param name="msg">Mensaje de error en caso de que no se encuentre el token.</param>
        /// <returns>El token consumido.</returns>
        /// <exception cref="Exception">Se lanza si el token no coincide con el tipo esperado.</exception>
        private Token Consumir(TipoToken tipo, string msg)
        {
            if (Match(tipo)) return Anterior();
            throw new Exception(msg);
        }


        /// <summary>
        ///  Verifica si el token actual coincide con el tipo esperado.
        /// </summary>
        /// <param name="tipo">Tipo de token esperado.</param>
        /// <returns>True si el token actual coincide con el tipo esperado, de lo contrario false.</returns>
        private bool Match(TipoToken tipo)
        {
            if (Check(tipo)) { Avanzar(); return true; }
            return false;
        }


        /// <summary>
        ///  Verifica si el token actual coincide con el tipo esperado sin consumirlo.
        /// </summary>
        /// <param name="tipo">Tipo de token esperado.</param>
        /// <returns>True si el token actual coincide con el tipo esperado, de lo contrario false.</returns>
        private bool Check(TipoToken tipo)
        {
            if (EsFin()) return false;
            return Actual().Tipo == tipo;
        }


        /// <summary>
        ///  Avanza al siguiente token y devuelve el token anterior.
        /// </summary>
        /// <returns>El token anterior al avance.</returns>
        private Token Avanzar()
        {
            if (!EsFin()) _posicionActual++;
            return Anterior();
        }






    }
}