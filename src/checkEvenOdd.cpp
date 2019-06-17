#include <iostream>
using namespace std;
bool checkEvenOdd(int num);

/* This function checks whether the passed number is even
 * or odd. If the number is even then this function returns
 * true else it returns false.
 */
bool checkEvenOdd(int num){
  bool b;
  /* If number is perfectly divisible by 2 then it is
   * an even number else it is an odd number
   *
   */
  if (num % 2 == 0)
    b=true;
  else
    b=false;

  return b;
}

/*** R
checkEvenOdd(susceptibility = 0.5, foi = 0.15, randnum_inf = 0.05)
*/
