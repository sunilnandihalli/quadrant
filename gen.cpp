#include <iostream>
#include <cstdlib>
using namespace std;
int nzrand()
{
  int r = 0;
  while(r==0)
    r = (rand()%100000);
  return r;
}
int plusOrMinux()
{
  return (rand()%2==0)?1:-1;
}
int main()
{
  int n = 100;
  cin>>n;
  cout<<n<<std::endl;
  for(int i=0;i<n;i++)
    {
      cout<<plusOrMinux()<<' '<<plusOrMinux()<<endl;
    }
  cout<<n<<std::endl;
  for(int i=0;i<n;++i)
    {
      char com[]="XYC";
      int n1 = nzrand()%n+1;
      int n2 = nzrand()%n+1;
      int t;
      if (n1>n2) 
	{
	  t = n1;
	  n1 = n2;
	  n2 = t;
	}
      std::cout<<com[rand()%3]<<' '<<n1<<' '<<n2<<std::endl;
    }
}
