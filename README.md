### Kotlin конспект

  ## Кому он нужен?
  Kotlin компилирует свой код используя JVM в байт код. Соответственно все те, кто знакомы с JVM могут найти применение Kotlin. Также возможна компиляция в язык JavaScript, что еще сильнее расширяет границы пользователей. 
  
  ## Совместимый язык
  Kotlin полностью совместим с Java, что позволяет бесшовно встраивать его код в уже существующие на Java проэкты. Разработчики языка утверждают, что он эффективнее чем Java, и что любой разработчик сможет перейти на Kotlin прочитав или посмотрев любой туториал, или спецификацию языка за пол часа.
  
  ## Особенности
  # NullSafety
  Язык позволяет указывать в параметрах функции, возможна ли передача в этот параметр ссылки на null.
  
  ```Kotlin
  /* вопросом помечаем, что может прийти null */
  fun someFunction(someNullableParam:SomeType?) {
      if(someNullableParam != null) {
           // smart cast. Компилятор видит, что передаваемое 
           // значение не null и разрешает его передать в функцию
          anotherFunction(someNullableParam)
      }
  }

  /* здесь же уже null не пройдет, в попытке передать 
   * null или nullable значение компилятор выдаст ошибку */
  fun anotherFunction(someParam:SomeType) {
      // делаем что-то без опаски, что переданное значение null
  }
  ```
  
  # Методы расширения
  Kotlin позволяет добавлять методы класса вне объявления самого класса.
  ```Kotlin
  interface Vector2 {

    val x:Float // это не поле, а ридонли свойство (property)
    val y:Float // в Java были бы методы getX() и getY()

}

/* Extension property. Без поля в классе, просто getLength() */
val Vector2.length:Float
    get() = (x * x + y  * y).sqrt() // притворимся, что такая extension-функция для Float уже существует

/* переопределяем оператор + */
operator fun Vector2.plus(other:Vector2):Vector2 = createVector(x+this.x, y+this.y) // какой-то способ создать новый вектор

/* без тела, после знака = пишем одно выражение */
fun Vector2.dot(x: Float, y: Float): Float = x * this.x + y * this.y

/* Помечая функцию с одним параметром как infix, 
 * мы позволяем вызывать её через пробел: v1 dot v2 */
infix fun Vector2.dot(vec2: Vector2): Float = dot(vec2.x, vec2.y)

fun usage(vec1:Vector2, vec2:Vector2) {

    val dotProduct = vec1 dot vec2
    val sum = vec1 + vec2 // на выходе новый вектор
    val length = sum.length // обращаемся просто как к свойству

}
```

# Лямбды
Ktolin имеет притензии называться языком функционального программирования, поэтому он имеет возможнотсть объявлять функцию не только внутри пакета, но и передавать её как параметр, возвращать её из функции и тд.
```Kotlin
/* передаем в одну функцию другую -- принимает в параметр Int
 * и Int же возвращает. Возвращаем её же, только с фиксированным 
 * значением в качестве параметра */
fun passTen(func: (Int)->Int ): ()->Int {
    return { func(10) }
}
```

# Extention лямбда
Позволяет создать лямбда фукнцию, которая будет ещё и Extention-функцией.
```Kotlin
class World(val name:String = "world")

val printName:World.()->Unit = { 
     // интерполяцией в синтаксисе языка сейчас уже 
     // никого не привлечь на темную сторону
    println("Hello $name")
}

val world = World()

 // вызываем нашу функцию как будто это метод нашего класса!
world.printName()
```

# Inline
Присутствуют inlnine функции
```Kotlin
inline fun lock(lock:Lock, block:()->Unit) {
    lock.lock()
    try { 
        block()
    } finally {
        lock.unlock()
    }
}

fun usage() {
    lock(Lock()) {
        // делаем что-то внутри блокировки
    }
}
```

# Делегирование

В котлине есть два типа делегирования. Первый, который позволяет делегировать все методы реализуемого интерфейса к какому-то инстансу этого типа:
```Kotlin
interface Connection {

    fun connect()

}

/* здесь мы видим стандартный для котлина синтаксис определения 
 * класса вместе с параметрами конструктора и свойствами -- 
 * в данном случае connection будет и в конструкторе и в поле. 
 * Есть возможность определить и множественный конструктор 
 * см https://kotlinlang.org/docs/reference/classes.html#constructors
 * И, наконец, мы видим что класс реализует интерфейс Connection, все методы
 * которого делегируются к переданному в конструктор экземпляру Connection-а.
 * При желании их конечно можно переопределить в теле класса */
class ConnectionWrapper(val connection:Connection) : Connection by connection
```


У этого синтаксиса есть ряд ограничений. Например, инстанс для делегирования должен быть известен до вызова конструктора.

Второй тип делегирования — это delegated properties. Позволяет определить объект с методами get (и set для var), к которым будет осуществляться делегирование доступа при обращении к свойству объекта.
```Kotlin
class Foo {
    
    /* это делегат из стандартной библиотеки, 
       позволяет отложить инициализацию поля 
       до первого обращения к нему */ 
    private val someProeprty by lazy { HavyType() }

}
```


# Деструктуризация
```Kotlin
val (first, second) = someFunc()
```
Чтобы такой код заработал, возвращаемое значение из someFunc() должно быть типа, у которого есть (можно extension) методы component1(), component2():
```Kotlin
class Foo {
    fun component1():String = "test"
    fun component2():Int = 10
}
fun someFunc():Foo = Foo()

// или так, to -- в это такой infix extension-метод определенный 
// для Any, который создает экземпляр класса Pair, метод hashMapOf
// в свою очередь принимает vararg параметр таких пар
val map = hashMapOf(1 to "test")
for ((id, name) in map) {
    //  такой синтаксис возможен, потому что для Map-а определен метод iterator()
    // возвращающий набор Map.Entry, а для него в свою очередь определены два
    // extension-метода component1() и component2()
}
```


