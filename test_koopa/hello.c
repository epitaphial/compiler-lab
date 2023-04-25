int main()
{
  int a = 20;
  {
    a = 10;
    int a = 2;
    int b = a+1;
  }
  return a;
}