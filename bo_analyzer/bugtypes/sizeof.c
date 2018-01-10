typedef struct _charVoid
{
  char charFirst[16];
  void * voidSecond;
  void * voidThird;
} charVoid;

int main()
{
  charVoid s;
  sparrow_print (sizeof(charVoid));  
  sparrow_print (sizeof(s)); 
  memcpy (s.charFirst, "123456789abcdefghij", sizeof(s));
}
