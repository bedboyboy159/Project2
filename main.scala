import java.io.PrintWriter
import java.io.File
import scala.collection.mutable.HashMap

var hashMap = HashMap(1->"I", 4->"IV", 9->"IX", 5->"V", 10->"X", 50->"L", 100->"C", 500->"D", 1000->"M");
var hashMap2 = HashMap("I"->1, "IV"->4, "IX"->9, "V"->5, "X"->10, "L"->50, "C"->100, "D"->500, "M"->1000);

object main
{
  def main( args: Array[String]): Unit =
    val inputfile = args(1)
    val outputfile = args(2)
    val out = new PrintWriter( new File(outputfile))
    out.printf("Arabic     <-->     Roman \n");
    scala.io.Source.fromFile(inputfile).getLines().foreach{ line =>
    if ( line != "" )
      var words = line.split(" ")
      if ( words(0) == "a2r" ){
        var result = words(1).toInt;
        var ans = art(result);
        out.printf("%s        --->", words(1));
        out.printf("     %s\n", ans);
      }else{
        val ans = rti(words(1));
        out.printf("%d       <---", ans);
        out.printf("      %s \n", words(1));
      }
    }
    out.close()

}

def art(num: Int) : String =
  if(hashMap.contains(num)){
    return hashMap(num);
  }
  else if(num % 40 == 0 && num < 1000) // 400, 40
    return art(num/4) + art(num + num / 4);
  else if(num % 90 == 0 && num < 1000 && num != 450) // 900, 90, 450
    return art(num/9) + art(num + num / 9);
  else if(2 <= num && num <= 3)
    return art(num - 1) + art(1);
  else if(6 <= num && num <= 8)
    return art(5) + art(num - 5);
  else if(11 <= num && num <= 39)
    return art(10) + art(num - 10);
  else if(41 <= num && num <= 49)
    return art(40) + art(num - 40);
  else if(51 <= num && num <= 89)
    return art(50) + art(num - 50);
  else if(91 <= num && num <= 99)
    return art(90) + art(num - 90);
  else if(101 <= num && num <= 399)
    return art(100) + art(num - 100);
  else if(401 <= num && num <= 499)
    return art(400) + art(num - 400);
  else if(501 <= num && num <= 899)
    return art(500) + art(num - 500);
  else if(901 <= num && num <= 999)
    return art(900) + art(num - 900);
  else
    return art(1000) + art(num - 1000);
    

def rti(roman: String) : Int =
  if(hashMap2.contains(roman)){
    return hashMap2(roman);
  }
  else{
    val first_index = roman.charAt(0).toString;
    val second_index = roman.charAt(1).toString;

    if(rti(first_index) < rti(second_index)){
      return (rti(second_index) - rti(first_index)) + rti(roman.substring(2));
    }else{
      return rti(first_index) + rti(roman.substring(1));
    }
  }
