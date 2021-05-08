import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;


public class StayHome {
    public static void main(String args[]) {
        ReadInput input = new ReadInput("C:\\Users\\Eleni K\\Desktop\\stayhome.in17");
        Covid after_covid = new Covid(input.N, input.M);
        int[][] world = after_covid.After_Covid(input.GetInput());
        Sotiris sotiris = new Sotiris (input.N, input.M, after_covid.S_i, after_covid.S_j, after_covid.T_i, after_covid.T_j);
        sotiris.Sotiris_Return(world);
    }
}
class ReadInput {
    public char[][] world = new char[1002][1002];
    public int N;
    public int M;

    ReadInput(String infile) {
        //creates the map of the world before covid
        try {
            BufferedReader in = new BufferedReader(new FileReader(infile));
            String line = null;
            int i = 0;
            while ((line = in.readLine()) != null) {
                if (i == 0) this.M = line.length();
                for (int j = 1; j <= this.M; j++) world[i + 1][j] = line.charAt(j - 1);
                i++;
            }
            in.close();
            this.N = i; //#line

            //Margins
            for (i = 0; i <= this.M + 1; i++) {
                world[0][i] = world[this.N + 1][i] = 'X';
            }
            for (i = 0; i <= this.N + 1; i++) {
                world[i][0] = world[i][this.M + 1] = 'X';
            }

            /*/Printing array
            for (i = 0; i <= this.N+1; i++) {
                for(int j = 0; j <= this.M+1; j++){
                    System.out.print(world[i][j]);
                }
                System.out.println();
            }*/
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    public char [][] GetInput() {
        //getter function, returns char [][] world
        return this.world;
    }
}

class Covid{
    int N;
    int M;
    int S_i; int S_j;
    int T_i; int T_j;
    Covid(int N, int M) {
        this.N = N;
        this.M = M;
        this.S_i = this.S_j = this.T_i = this.T_j = -1;
    }

    public int [][] After_Covid(char[][] before_covid){
        //creates the map of the world, each cell contains
        // the moment covid arrives to each place
        int [][] world = new int [this.N+2][this.M+2];
        ArrayList<Tuple> queue1 = new ArrayList<Tuple>();
        Iterator it1 = queue1.iterator();
        for(int i = 0; i <= this.N+1; i++) {
            for(int j = 0; j <= this.M+1; j++) {
                if (before_covid[i][j] == 'X') world[i][j] = -1;
                else if (before_covid[i][j] == 'W') {
                    world[i][j] = 0;
                    queue1.add(new Tuple(i, j, 0));
                }
            }
        }
        /*System.out.println("Print queue1...");
        for(Tuple t : queue1)
            System.out.println(t);*/
        boolean covid_in_airport = false;
        int infect_airports = 0;
        int countdown = -1;
        while(!queue1.isEmpty() || countdown >= 0){
            /*System.out.println("Print queue1...");
            for(Tuple t : queue1)
                System.out.println(t);*/
            ArrayList<Tuple> queue2 = new ArrayList<Tuple>();
            if(covid_in_airport == true && countdown > 0) countdown--;
            else if(covid_in_airport == true && countdown == 0) {
                countdown = -1;
                for(int i = 1; i <= N; i++)
                    for(int j = 1; j <= M; j++)
                        if(before_covid[i][j] == 'A'){
                            before_covid[i][j] = 'X';
                            world[i][j] = infect_airports;
                            queue2.add(new Tuple(i, j, infect_airports));
                        }
            }
            while(!queue1.isEmpty()){
                Tuple<Integer> curr = queue1.get(0);
                queue1.remove(0);
                int curr_i = curr.x;
                int curr_j = curr.y;
                int curr_t = curr.z;
                int next_time = curr_t + 2;
                ArrayList<Tuple> neighbors = Neighbors(before_covid, curr_i, curr_j);
                Iterator it = neighbors.iterator();
                while(it.hasNext()) {
                    Tuple t = (Tuple) it.next();
                    int neigh_i = t.x;
                    int neigh_j = t.y;
                    char s = before_covid[neigh_i][neigh_j];
                    if (s == '.' || s == 'A' || s == 'S' || s == 'T') {
                        if (s == 'S') {
                            S_i = neigh_i;
                            S_j = neigh_j;
                        } else if (s == 'T') {
                            T_i = neigh_i;
                            T_j = neigh_j;
                        } else {
                            if (s == 'A' && covid_in_airport == false) {
                                covid_in_airport = true;
                                infect_airports = next_time + 5;
                                countdown = 2;
                            }
                        }
                        before_covid[neigh_i][neigh_j] = 'X';
                        world[neigh_i][neigh_j] = next_time;
                        queue2.add(new Tuple(neigh_i, neigh_j, next_time));
                    }
                }
            }
            queue1 = queue2;
        }
        /*/Printing array
            for (int i = 0; i <= this.N+1; i++) {
                for(int j = 0; j <= this.M+1; j++){
                    System.out.print(world[i][j] + " ");
                }
                System.out.println();
            }*/
            return world;
    }
    ArrayList<Tuple> Neighbors(char[][] world, int i, int j){
        //return a list of all neighbors
        ArrayList<Tuple> neighbors = new ArrayList<Tuple>();
        if(world[i+1][j] != 'X'){
            neighbors.add(new Tuple(i+1, j, 'D'));
        }
        if(world[i][j-1] != 'X'){
            neighbors.add(new Tuple(i, j-1, 'L'));
        }
        if(world[i][j+1] != 'X'){
            neighbors.add(new Tuple(i, j+1, 'R'));
        }
        if(world[i-1][j] != 'X'){
            neighbors.add(new Tuple(i-1, j, 'U'));
        }
        return neighbors;
    }
}

class Sotiris {
    int N;
    int M;
    int S_i; int S_j;
    int T_i; int T_j;

    Sotiris(int N, int M, int S_i, int S_j, int T_i, int T_j) {
        this.N = N;
        this.M = M;
        this.S_i = S_i; this.S_j = S_j;
        this.T_i = T_i; this.T_j = T_j;
    }
    ArrayList<Tuple> Neighbors(int [][] world, int i, int j){
        //return a list of all neighbors
        ArrayList<Tuple> neighbors = new ArrayList<Tuple>();
        if(world[i+1][j] != -1){
            neighbors.add(new Tuple(i+1, j, 'D'));
        }
        if(world[i][j-1] != -1){
            neighbors.add(new Tuple(i, j-1, 'L'));
        }
        if(world[i][j+1] != -1){
            neighbors.add(new Tuple(i, j+1, 'R'));
        }
        if(world[i-1][j] != -1){
            neighbors.add(new Tuple(i-1, j, 'U'));
        }
        return neighbors;
    }
    public void Sotiris_Return(int[][] after_covid) {
        //notes all possibles Sotiris's paths
        //and prints the final result
        ArrayList<Tuple> queue1 = new ArrayList<Tuple>();
        queue1.add(new Tuple<Character>(S_i, S_j, '-'));
        after_covid[S_i][S_j] = -1;
        Tuple<Character>[][] world;
        world = new Tuple[N + 2][M + 2];
        for (int i = 0; i <= N + 1; i++) {
            for (int j = 0; j <= M + 1; j++) {
                world[i][j] = new Tuple(0, 0, '-');
                if (after_covid[i][j] == -1) world[i][j] = null;
            }
        }
        int counter = 0;
        boolean stayhome = false;
        while (!queue1.isEmpty() && stayhome == false) {
            counter++;
            ArrayList<Tuple> queue2 = new ArrayList<Tuple>();
            while (!queue1.isEmpty()) {
                Tuple<Character> curr = queue1.get(0);
                queue1.remove(0);
                int curr_i = curr.x;
                int curr_j = curr.y;
                ArrayList<Tuple> neighbors = Neighbors(after_covid, curr_i, curr_j);
                Iterator it = neighbors.iterator();
                while (it.hasNext()) {
                    Tuple t = (Tuple) it.next();
                    int neigh_i = t.x;
                    int neigh_j = t.y;
                    int s = after_covid[neigh_i][neigh_j];
                    if (s > counter) {
                        if (neigh_i == T_i && neigh_j == T_j) stayhome = true;
                        after_covid[neigh_i][neigh_j] = -1;
                        world[neigh_i][neigh_j] = new Tuple(curr_i, curr_j, t.z);
                        queue2.add(new Tuple(neigh_i, neigh_j, '-'));
                    }
                }
            }
            queue1 = queue2;
        }
        if (stayhome == false) System.out.println("IMPOSSIBLE");
        else {
            System.out.println(counter);
            ArrayList<Character> path = new ArrayList<Character>();
            int i = T_i;
            int j = T_j;
            while (true) {
                if (i == S_i && j == S_j) break;
                path.add(world[i][j].z);
                int i_aux = i;
                i = world[i][j].x;
                j = world[i_aux][j].y;
            }
                Collections.reverse(path);
                for (char c : path)
                    System.out.print(c);
                System.out.println();
        }
    }
}

class Tuple<T>{
    int x;
    int y;
    T z;
    Tuple (int x, int y, T z){
        this.x = x;
        this.y = y;
        this.z = z;
    }
    public String toString(){
        return "("+ this.x + "," + this.y + "," + this.z + ")";
    }
}
