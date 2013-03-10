package cz.etn.scw3


object fibws {

	abstract class Person(name: String, age: Int)
	case class Man(name: String, age: Int, favoritePerfume: String) extends Person(name, age)
	case class Woman(name: String, age: Int, favoriteCar: String) extends Person(name, age)
	
	val pepa = Man("Pepa", 30, "Channel")     //> pepa  : cz.etn.scw3.fibws.Man = Man(Pepa,30,Channel)
	val person: Person = pepa                 //> person  : cz.etn.scw3.fibws.Person = Man(Pepa,30,Channel)
  
  val tonda @ Man(tName, tAge, tPerfume) = pepa.copy(name = "Tonda", age = 50)
                                                  //> tonda  : cz.etn.scw3.fibws.Man = Man(Tonda,50,Channel)
                                                  //| tName  : String = Tonda
                                                  //| tAge  : Int = 50
                                                  //| tPerfume  : String = Channel
  
  val perfume =
    if (person.isInstanceOf[Man])
  	 person.asInstanceOf[Man].favoritePerfume
    else "Nic"                                    //> perfume  : String = Channel
    
  person match {
    case Man("Pepa", _, perfume) => perfume
    case _ => "Nic"
  }                                               //> res0: String = Channel
  
  Fib(10)                                         //> res1: Long = 34
  Fib(40)                                         //> res2: Long = 63245986
}