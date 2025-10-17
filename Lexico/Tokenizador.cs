using System;
using System.Collections.Generic;

namespace AnalizadorLexico.Lexico
{
    /// <summary>
    /// Clase responsable de tokenizar el código fuente de FORTRAN77.
    /// Esta clase analiza el texto de entrada y genera una lista de tokens.
    /// </summary>
    public class Tokenizador
    {
        // TODO: Definir tipos de tokens (palabras clave, identificadores, operadores, etc.)
        // TODO: Implementar método para analizar el código fuente y generar tokens
        // TODO: Manejar errores léxicos (caracteres inválidos, etc.)

        /// <summary>
        /// Tokeniza el código fuente proporcionado.
        /// </summary>
        /// <param name="codigoFuente">El código fuente a tokenizar.</param>
        /// <returns>Lista de tokens generados.</returns>
        public List<Token> Tokenizar(string codigoFuente)
        {
            // Implementación pendiente
            throw new NotImplementedException();
        }
    }

    /// <summary>
    /// Representa un token en el análisis léxico.
    /// </summary>
    public class Token
    {
        // TODO: Agregar propiedades como Tipo, Valor, Línea, Columna, etc.
    }
}