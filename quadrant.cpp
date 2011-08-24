#include <iostream>
#include <vector>
using namespace std;

int main()
{
  int n,q;
  cin>>n;
  std::vector<bool> X(n),Y(n);
  for(int i=0;i<n;i++)
    {
      int x,y;
      cin>>x>>y;
      X[i]=(x>0);
      Y[i]=(y>0);
    }
  cin>>q;
  for(int i=0;i<q;i++)
    {
      char c;
      int start,end;
      cin>>c>>start>>end; start--;
      switch (c)
	{
	case 'X': 
	  {
	    for(int i = start; i<end;i++)
	      Y[i] = !Y[i];
	  }
	  break;
	case 'Y':
	  { 
	    for(int i=start;i<end;i++)
	      X[i]=!X[i];
	  }
	  break;
	case 'C':
	  { int q1(0),q2(0),q3(0),q4(0);
	    for(int i=start;i<end;i++)
	      if(X[i])
		if(Y[i]) q1++; else q4++;
	      else 
		if(Y[i]) q2++; else q3++;
	    std::cout<<q1<<" "<<q2<<" "<<q3<<" "<<q4<<std::endl;
	  }
	}
    }	
}
