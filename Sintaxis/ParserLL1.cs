using System;
using System.Collections.Generic;
using AnalizadorLexico.Lexico;

namespace AnalizadorLexico.Sintaxis
{
    /// <summary>
    /// Clase que implementa un parser LL(1) para el subconjunto de FORTRAN77.
    /// Esta clase toma una lista de tokens y construye un árbol sintáctico.
    /// </summary>
    public class ParserLL1
    {
        // TODO: Definir gramática LL(1) para FORTRAN77
        // TODO: Implementar tabla de análisis sintáctico
        // TODO: Manejar errores sintácticos

        /// <summary>
        /// Parsea la lista de tokens y genera un árbol sintáctico.
        /// </summary>
        /// <param name="tokens">Lista de tokens a parsear.</param>
        /// <returns>Árbol sintáctico resultante.</returns>
        public ArbolSintactico Parsear(List<Token> tokens)
        {
            // Implementación pendiente
            throw new NotImplementedException();
        }
    }
}