/*
Project     : Programming Languages 1 - Assignment 1 - Exercise 2 
Author      : Eleni-Elpida Kapsali (eleni_kaps@hotmail.com)
Date        : April 29, 2020
Description : Coronographs (C++ code)
------------
School of ECE, National Technical University of Athens
*/
#include<stdio.h> 
#include <list> 
#include <limits.h>
#include<vector> 
#include <bits/stdc++.h>
using namespace std; 

//Coronographs: connected + exactly one cycle
//A connected udirected graph has exactly one cycle iff |V|=|E| 

//Class of an undirected graph (using simple-linked lists)
class Graph{
public: 
    Graph(int V, int E);   // Constructor 
    void addEdge(int v, int w);   // add an edge to the graph 
    void Corona();
private: 
    int V; //number of vertexes
    int E; //number of edges
    bool FindCycle_aux (vector<bool> &visited, vector<int> &incycle);
    int cntNodes(int v, vector<bool> &counted, vector<bool> &in_cycle);
	list<int> *adj;
};

Graph::Graph(int V, int E) 
{ 
    this->V = V; 
    this->E = E;
    adj = new list<int>[V]; 
} 
  
void Graph::addEdge(int v, int w) 
{ 
    adj[(v-1)].push_back(w-1); // Add w to v’s list. 
    adj[(w-1)].push_back(v-1); // Add v to w’s list. 
    //Vertexes are enumerated from 1 and we save
   // the list of vertex 1 in adj[0]
}




bool Graph::FindCycle_aux (vector<bool> &visited, vector<int> &incycle){
    vector<bool> instack(V, false);
    vector<int> prev(V, -1);
    stack<int> stack;
    stack.push(0);
    instack[0] = true;
    visited[0] = true;
    int s, c1, c2;
    bool cycle  = false;
    list<int>::iterator i;
    while(!stack.empty()){
    	s = stack.top();
    	instack[s] = false;
		stack.pop();
        for (i = adj[s].begin(); i != adj[s].end(); ++i){
        	if(!visited[*i]){
        		prev[*i] = s;
        		visited[*i] = true;
        		instack[*i] = true;
        		stack.push(*i);
        	}
        	else if(instack[*i]){
        		cycle = true;
        		c1 = *i; c2 = s;
        		incycle.push_back(c1);
        		printf("c1 %d\n", c1);
        		printf("c2 %d\n", c2);
        		incycle.push_back(c2);
        	}
        }
    }
    if(cycle){
    	int j = c2;
    	do{
    		j = prev[j];
    		incycle.push_back(j);
			}while(j != prev[c1]);	
		return true;
	}
	else return false;
}
    	
int Graph::cntNodes(int v, vector<bool> &counted, vector<bool> &in_cycle){
    stack<int> stack;
    stack.push(v);
    counted[v] = true;
    int s;
    int cnt = 0;
    list<int>::iterator i;
    while(!stack.empty()){
    	s = stack.top();
		stack.pop();
        for (i = adj[s].begin(); i != adj[s].end(); ++i){
        	if(!counted[*i] && !in_cycle[*i]){
        		cnt++;
        		counted[*i] = true;
        		stack.push(*i);
        	}
        }
    }	
    return cnt;
}

void Graph::Corona(){
    if(V != E){
	 printf("NO CORONA\n");
	 return;	
   }
	vector<bool> visited(V,false);
	vector<int> incycle;
    bool cycle = FindCycle_aux (visited, incycle);
	bool connected = true;
	vector<bool>::iterator i;
    for (i = visited.begin(); i < visited.end(); i++){
	  if (!(*i)){
	  	connected = false;
	  	break;
	  }
   }
   if(cycle && connected) {
    printf("CORONA %lu\n", incycle.size());
    vector<bool> in_cycle(V,false);
    vector<int>::iterator i;
    for (i = incycle.begin(); i < incycle.end(); i++) in_cycle[*i] = true;
    vector<bool> counted(V, false);
    vector<int> cntnodes;
    for(long unsigned int v=0; v < incycle.size(); v++){
    	int x = cntNodes (incycle[v], counted, in_cycle);
    	cntnodes.push_back(x + 1);
    }
    sort(cntnodes.begin(), cntnodes.end());
    for(i = cntnodes.begin(); i != (cntnodes.end() - 1); ++i) printf("%d ", *i);
    printf("%d\n", *i);
   	}
   else printf("NO CORONA\n");
    }
    
int main(int argc, char* argv[]){
FILE *fptr;
fptr = fopen("mycorona.in1", "r");
if(fptr == NULL) printf("Error in opening file");
else{
	int T, V, E;
	fscanf(fptr, "%d", &T);
    for(int i = 0; i < T; i++){
    	fscanf(fptr, "%d", &V);
    	fscanf(fptr, "%d", &E);
    	Graph g(V, E);
    	int v, w;
    	for(int i = 0; i < E; i++){
    		fscanf(fptr, "%d", &v);
    		fscanf(fptr, "%d", &w);
    		g.addEdge(v, w);
    	}
    	g.Corona();
    }
}
return 0;
}
	
	
