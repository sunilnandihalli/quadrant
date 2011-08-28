#include <iostream>
#include <vector>
#include <list>
#include <deque>
#include <fstream>


using namespace std;
class node;
class childInfo;

std::vector<node*> memory;
std::vector<childInfo*> cInfoMemory;
std::vector<bool> X,Y;
int n;
std::ifstream fin;

enum qmaptype {NONE=0,FLIPX=1,FLIPY=2,FLIPXY=3};

inline qmaptype compose(qmaptype qmap1,qmaptype qmap2)
{
  return qmaptype(qmap1^qmap2);
}

class childInfo
{
public:
  void init(node* _l,node* _r);
  float mid;
  qmaptype qmap;
  node *l,*r;
};
inline void addQuadrantCounts(int* q,int* ql,int* qr)
{
  q[0]=ql[0]+qr[0];
  q[1]=ql[1]+qr[1];
  q[2]=ql[2]+qr[2];
  q[3]=ql[3]+qr[3];
}
class node
{
public:
  node()
  {cinfo=NULL;}
  void init(childInfo* _cinfo)
  {
    cinfo = _cinfo;
    start = cinfo->l->start;
    end = cinfo->r->end;
    int* ql = cinfo->l->qcount;
    int* qr = cinfo->r->qcount;
    addQuadrantCounts(qcount,ql,qr);
  }
  void init(int id,int quadrantId)
  {
    cinfo=NULL;
    start=end=id;
    qcount[0]=qcount[1]=qcount[2]=qcount[3]=0;
    qcount[quadrantId]=1;
  }
  int start,end;
  int qcount[4];
  childInfo* cinfo;
};

void childInfo::init(node* _l,node* _r)
{
  l = _l;
  r = _r;
  qmap=NONE;
  mid = (l->end+r->start)/2.0;
}
inline int mswap(int& a,int& b)
{
  int tmp = a;
  a = b;
  b = tmp;
}

inline void swapX(int* q)
{
  mswap(q[0],q[3]);
  mswap(q[1],q[2]);
}

inline void swapY(int* q)
{
  mswap(q[0],q[1]);
  mswap(q[2],q[3]);
}

inline void applyTransformation(int* q,qmaptype qm)
{
  switch (qm)
    {
    case NONE: break;
    case FLIPX: swapX(q); break;
    case FLIPY: swapY(q); break;
    case FLIPXY: swapX(q); swapY(q); break;
    }
}

node* createTree()
{
  node* cNodeInfo = new node[n];
  node* cNode = cNodeInfo;
  memory.push_back(cNodeInfo);
  for(int i=0;i<n;i++)
    {
      int quadrantId;
      if (X[i]) 
	if(Y[i]) quadrantId = 0; else quadrantId = 3;
      else 
	if(Y[i]) quadrantId = 1; else quadrantId = 2;	
      cNode->init(i,quadrantId);
      cNode++;
    }
  node* carryNode = NULL;
  node* cNodeCur = cNodeInfo;
  while(n!=1)
    {
      bool willCreateCarry = (n%2 ==1);
      int newN =willCreateCarry?((n+1)/2):(n/2);
      if(willCreateCarry && !carryNode) carryNode = cNodeCur+(n-1);
      node* cNodePrev = cNodeCur;

      int newNodes = n/2;
      
      childInfo* cinfoptr = new childInfo[newNodes];
      cInfoMemory.push_back(cinfoptr);
      cNodeCur = new node[newNodes];
      node* cNodeCurStart = cNodeCur;
      memory.push_back(cNodeCur);
      int defaultNewNodes=(!willCreateCarry && carryNode)?(newNodes-1):newNodes;
      for(int i=0;i<defaultNewNodes;++i)
	{
	  node* cNodePrevPrev=cNodePrev;
	  cNodePrev++;
	  cinfoptr->init(cNodePrevPrev,cNodePrev);
	  cNodeCur->init(cinfoptr);
	  ++cNodeCur;
	  ++cNodePrev;
	  ++cinfoptr;
	}
      if(!willCreateCarry && carryNode)
	{
	  cinfoptr->init(cNodePrev,carryNode);
	  cNodeCur->init(cinfoptr);
	  carryNode=NULL;
	}
      cNodeCur = cNodeCurStart;
      n=newN;
    }
  return cNodeCur;
}

node* readPoints()
{
  cin>>n;
  X.resize(n);
  Y.resize(n);
  for(int i=0;i<n;i++)
    {
      int x,y;
      cin>>x>>y;
      X[i]=(x>0);
      Y[i]=(y>0);
    }
  return createTree();
}

void helper(int* retQCount,node* t,qmaptype parentOutOfRangeMapper,qmaptype parentInRangeMapper,int s,int e)
{
  qmaptype outOfRangeMapper,inRangeMapper;
  qmaptype cqmap = (t->cinfo)?t->cinfo->qmap: NONE;
  outOfRangeMapper=compose(parentOutOfRangeMapper,cqmap);
  inRangeMapper=compose(parentInRangeMapper,cqmap);
  if (-1==s) 
    {
      applyTransformation(t->qcount,parentOutOfRangeMapper);
      if(t->cinfo) 
	t->cinfo->qmap=outOfRangeMapper;
      retQCount[0]=retQCount[1]=retQCount[2]=retQCount[3]=0;
    }
  else if ((s==t->start)&&(e==t->end))
    {
      applyTransformation(t->qcount,parentInRangeMapper);
      if(t->cinfo)
	t->cinfo->qmap=inRangeMapper;
      int* r=retQCount; 
      int* q=t->qcount;
      r[0]=q[0],r[1]=q[1],r[2]=q[2],r[3]=q[3];
    } 
  else 
    {
      int ql[4],qr[4];
      float m=t->cinfo->mid;
      int rightS,rightE,leftS,leftE;
      if((s<m)&&(m<e))
	{
	  int mint = int(m);
	  leftS = s,leftE=mint;
	  rightS = mint+1,rightE=e;
	}
      else if(e<m)
	{
	  leftS=s,leftE=e;
	  rightS=rightE=-1;
	}
      else if(m<s)
	{
	  leftS=leftE=-1;
	  rightS=s,rightE=e;
	}
      helper(ql,t->cinfo->l,outOfRangeMapper,inRangeMapper,leftS,leftE);
      helper(qr,t->cinfo->r,outOfRangeMapper,inRangeMapper,rightS,rightE);
      addQuadrantCounts(t->qcount,t->cinfo->l->qcount,t->cinfo->r->qcount);
      addQuadrantCounts(retQCount,ql,qr);
      t->cinfo->qmap=NONE;
    }
}

void processQuery(node* tree,char c,int s,int e)
{
  //std::cout<<" processing "<<c<<' '<<s<<' '<<e<<std::endl;
  int q[4];
  qmaptype qmap;
  switch(c)
  {
  case 'X': qmap=FLIPX;break;
  case 'Y': qmap=FLIPY;break;
  case 'C': qmap=NONE;break;
  }
  helper(q,tree,NONE,qmap,s,e);
  if(c=='C')
    std::cout<<q[0]<<' '<<q[1]<<' '<<q[2]<<' '<<q[3]<<std::endl;
}

int processQueries(node* tree)
{
  int q;
  cin>>q;
  for(int i=0;i<q;i++)
    {
      char c;
      int start,end;
      cin>>c>>start>>end;
      start--;
      end--;
      processQuery(tree,c,start,end);
    }
}

int main(int argc,char** argv)
{
  //fin.open(argv[1]);
  node* tree = readPoints();
  processQueries(tree);
}
