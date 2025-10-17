namespace AnalizadorLexico.Sintaxis
{
    using System.Collections.Generic;

    /// <summary>
    /// Clase base para nodos del árbol sintáctico.
    /// </summary>
    public abstract class NodoSintactico
    {
        public abstract string Tipo { get; }
    }


    /// <summary>
    /// Nodo que representa un programa completo.
    /// </summary>
    public class Programa : NodoSintactico
    {
        public override string Tipo => "Programa";
        public List<NodoSintactico> Sentencias { get; } = new();
    }


    /// <summary>
    /// Nodo que representa una asignación de variable.
    /// </summary>
    public class Asignacion : NodoSintactico
    {
        public override string Tipo => "Asignacion";
        public string Identificador { get; }
        public Expresion Valor { get; }

        public Asignacion(string identificador, Expresion valor)
        {
            Identificador = identificador;
            Valor = valor;
        }
    }

    /// <summary>
    /// Nodo base para expresiones.
    /// </summary>
    public abstract class Expresion : NodoSintactico { }


    /// <summary>
    /// Nodo que representa un número (entero o real).
    /// </summary>
    public class Numero : Expresion
    {
        public override string Tipo => "Numero";
        public string Lexema { get; }
        public Numero(string lexema) => Lexema = lexema;
    }


        /// <summary>  
        /// Nodo que representa una variable (identificador).
        /// </summary>
    public class Variable : Expresion
    {
        public override string Tipo => "Variable";
        public string Nombre { get; }
        public Variable(string nombre) => Nombre = nombre;
    }

    /// <summary>
    /// Nodo que representa una expresión binaria (operación entre dos expresiones).
    /// </summary>
    public class Binaria : Expresion
    {
        public override string Tipo => "Binaria";
        public string Operador { get; }
        public Expresion Izquierda { get; }
        public Expresion Derecha { get; }

        public Binaria(string operador, Expresion izquierda, Expresion derecha)
        {
            Operador = operador;
            Izquierda = izquierda;
            Derecha = derecha;
        }
    }

    /// <summary>
    /// Representa el árbol sintáctico completo.
    /// </summary>
    public class ArbolSintactico
    {
        public Programa Raiz { get; set; } = new Programa();
    }
}