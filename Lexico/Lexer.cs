using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;

namespace AnalizadorLexico.Lexico
{

    /// <summary>
    /// Clase que implementa un analizador léxico para un subconjunto de FORTRAN77.
    /// Esta clase toma el código fuente como entrada y genera una secuencia de tokens.
    /// </summary>
    public sealed class Lexer
    {
        private readonly string _codigoFuente;
        private int _posicion;
        private int _linea = 1;
        private int _columna = 1;

        /// <summary>
        /// Palabras reservadas de FORTRAN77. 
        /// </summary>
        private static readonly HashSet<string> Reservadas = new(
            new[] { "IF", "THEN", "ELSE", "ENDIF", "END", "DO", "CONTINUE", "GOTO", "STOP", 
                    "INTEGER", "REAL", "PROGRAM", "PRINT", "TO" });



        /// <summary>
        /// Inicializa una nueva instancia del analizador léxico con el código fuente proporcionado.
        /// </summary>
        /// <param name="codigoFuente">Código fuente a analizar.</param>
        public Lexer(string codigoFuente) => _codigoFuente = codigoFuente ?? string.Empty;



        /// <summary>
        /// Tokeniza el código fuente y genera una secuencia de tokens.
        /// </summary>
        /// <returns>Secuencia de tokens generados.</returns>
        public IEnumerable<Token> Tokenizar()
        {
            while (!EsFin())
            {
                var caracter = CaracterActual();
                if (char.IsWhiteSpace(caracter))
                {
                    if (caracter == '\n')
                    {
                        yield return new Token(TipoToken.EOL, "\\n", _linea, _columna);
                        _linea++;
                        _columna = 1;
                        _posicion++;
                        continue;
                    }
                    Avanzar();
                    continue;
                }

                // Reconocer operadores relacionales de FORTRAN77: .GT., .LT., etc.
                if (caracter == '.' && !SiguienteEsDigito())
                {
                    var opRelacional = LeerOperadorRelacional();
                    if (opRelacional != null)
                    {
                        yield return opRelacional;
                        continue;
                    }
                }

                if (char.IsLetter(caracter))
                {
                    yield return LeerIdentificadorOPalabraReservada();
                    continue;
                }

                if (char.IsDigit(caracter) || (caracter == '.' && SiguienteEsDigito()))
                {
                    yield return LeerNumero();
                    continue;
                }

                switch (caracter)
                {
                    case '+':
                    case '-':
                    case '*':
                    case '/':
                        yield return Emitir(TipoToken.OperadorAritmetico, caracter.ToString()); Avanzar(); break;
                    case '=':
                        yield return Emitir(TipoToken.Asignacion, "="); Avanzar(); break;
                    case '(':
                        yield return Emitir(TipoToken.ParentesisApertura, "("); Avanzar(); break;
                    case ')':
                        yield return Emitir(TipoToken.ParentesisCierre, ")"); Avanzar(); break;
                    case ',':
                        yield return Emitir(TipoToken.Coma, ","); Avanzar(); break;
                    case '\'':
                    case '"':
                        yield return LeerStringLiteral();
                        break;
                    default:
                        yield return Emitir(TipoToken.Desconocido, caracter.ToString()); Avanzar(); break;
                }
            }

            yield return new Token(TipoToken.EOF, string.Empty, _linea, _columna);
        }


        /// <summary>
        /// Lee un identificador o una palabra reservada del código fuente.
        /// </summary>
        /// <returns>Retorna un token que representa el identificador o la palabra reservada.</returns>
        private Token LeerIdentificadorOPalabraReservada()
        {
            var columnaInicio = _columna;
            var constructorCadena = new StringBuilder();
            //Mientras sea letra o dígito o _
            while (!EsFin() && (char.IsLetterOrDigit(CaracterActual()) || CaracterActual() == '_'))
            {
                //agrega el caracter actual al StringBuilder
                constructorCadena.Append(CaracterActual());
                Avanzar();
            }
            //transforma el StringBuilder a mayúsculas
            var lexema = constructorCadena.ToString().ToUpperInvariant();
            //determina si es palabra reservada o identificador
            var tipo = Reservadas.Contains(lexema) ? TipoToken.PalabraReservada : TipoToken.Identificador;
            //retorna el token correspondiente
            return new Token(tipo, lexema, _linea, columnaInicio);
        }


        /// <summary>
        /// Lee un operador relacional de FORTRAN77 (.GT., .LT., .EQ., .NE., .GE., .LE.)
        /// </summary>
        /// <returns>Token del operador relacional, o null si no es un operador válido.</returns>
        private Token? LeerOperadorRelacional()
        {
            if (CaracterActual() != '.') return null;
            
            var columnaInicio = _columna;
            var posicionInicio = _posicion;
            var constructorCadena = new StringBuilder();
            
            // Leer el punto inicial
            constructorCadena.Append(CaracterActual());
            Avanzar();
            
            // Leer letras dentro del operador
            while (!EsFin() && char.IsLetter(CaracterActual()))
            {
                constructorCadena.Append(CaracterActual());
                Avanzar();
            }
            
            // Verificar punto final
            if (!EsFin() && CaracterActual() == '.')
            {
                constructorCadena.Append(CaracterActual());
                Avanzar();
                
                var lexema = constructorCadena.ToString().ToUpperInvariant();
                // Validar si es un operador relacional conocido
                if (lexema == ".GT." || lexema == ".LT." || lexema == ".EQ." || 
                    lexema == ".NE." || lexema == ".GE." || lexema == ".LE.")
                {
                    return new Token(TipoToken.OperadorRelacional, lexema, _linea, columnaInicio);
                }
            }
            
            // Si no es un operador válido, retroceder
            _posicion = posicionInicio;
            _columna = columnaInicio;
            return null;
        }


        /// <summary>
        /// Lee un número (entero o real) del código fuente.
        /// </summary>
        private Token LeerNumero()
        {
            var columnaInicio = _columna;
            var constructorCadena = new StringBuilder();
            bool tienePunto = false;

            //Mientras no sea el fin del código fuente
            while (!EsFin())
            {
                var caracter = CaracterActual();
                // si es un dígito, lo agrega al StringBuilder
                if (char.IsDigit(caracter)) { constructorCadena.Append(caracter); Avanzar(); }
                // sino si es un punto y no se ha encontrado otro punto antes, lo agrega
                else if (caracter == '.' && !tienePunto) { tienePunto = true; constructorCadena.Append(caracter); Avanzar(); }
                else break;
            }

            var lexema = constructorCadena.ToString();
            // determina si es un número entero o real
            var tipo = tienePunto ? TipoToken.NumeroReal : TipoToken.NumeroEntero;
            //devuelve el token correspondiente
            return new Token(tipo, lexema, _linea, columnaInicio);
        }


        /// <summary>
        /// Verifica si el siguiente carácter es un dígito.
        /// </summary>
        private bool SiguienteEsDigito() => (_posicion + 1 < _codigoFuente.Length) && char.IsDigit(_codigoFuente[_posicion + 1]);

        /// <summary>
        /// Lee un string literal delimitado por comillas simples (') o dobles (").
        /// En FORTRAN77, los strings se delimitan con comillas simples.
        /// </summary>
        /// <returns>Token que representa el string literal.</returns>
        private Token LeerStringLiteral()
        {
            var columnaInicio = _columna;
            var comillaInicial = CaracterActual(); // ' o "
            var constructorCadena = new StringBuilder();
            
            // No incluir la comilla inicial en el lexema
            Avanzar();
            
            // Leer hasta encontrar la comilla de cierre
            while (!EsFin() && CaracterActual() != comillaInicial)
            {
                // En FORTRAN77, las comillas dobles se escapan con dos comillas seguidas
                if (CaracterActual() == comillaInicial && !EsFin() && _posicion + 1 < _codigoFuente.Length && _codigoFuente[_posicion + 1] == comillaInicial)
                {
                    constructorCadena.Append(comillaInicial);
                    Avanzar(); // Primera comilla
                    Avanzar(); // Segunda comilla
                }
                else
                {
                    constructorCadena.Append(CaracterActual());
                    Avanzar();
                }
            }
            
            // Consumir la comilla final
            if (!EsFin() && CaracterActual() == comillaInicial)
            {
                Avanzar();
            }
            
            return new Token(TipoToken.StringLiteral, constructorCadena.ToString(), _linea, columnaInicio);
        }


        /// <summary>
        /// Crea un nuevo token con el tipo y lexema especificados. 
        /// </summary>
        private Token Emitir(TipoToken tipo, string lexema) => new(tipo, lexema, _linea, _columna);


        /// <summary>
        /// Obtiene el carácter actual en la posición del analizador léxico.
        /// </summary>
        /// <returns>devuelve el carácter actual.</returns>
        private char CaracterActual() => _codigoFuente[_posicion];

        
        /// <summary>
        /// Verifica si se ha alcanzado el fin del código fuente.
        /// </summary>
        /// <returns>true si se ha alcanzado el fin; de lo contrario, false.</returns>
        private bool EsFin() => _posicion >= _codigoFuente.Length;

        /// <summary>
        /// Avanza la posición del analizador léxico al siguiente carácter.
        /// </summary>
        private void Avanzar()
        {
            if (EsFin()) return;
            if (_codigoFuente[_posicion] == '\n') { _linea++; _columna = 1; _posicion++; return; }
            _posicion++; _columna++;
        }
    }

}