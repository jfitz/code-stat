/*Scala program to demonstrate example of multiple 
variables declarations and assignments.*/

object ExampleVarDecAndAssin {
   def main(args: Array[String]) {
      
      var (name: String, age: Int) = Pair("Mike",21);

      //print values
      println("Name:   "+name);
      println("Age:    "+age);
      
      //declaration without specifying data type
      var (address,mobile)=Pair("New Delhi, India",1234567890);
      
      //print values
      println("Address:   "+address);
      println("Mobile:    "+mobile);      
            
   }
}