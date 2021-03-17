import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;
import java.util.ArrayDeque;
import java.util.ArrayList;

public class Vaccine{
    public static void main (String args[]){
        try {
            BufferedReader in = new BufferedReader(new FileReader(args[0]));
            String line = in.readLine();
            String [] a = line.split(" ");
            int N = Integer.parseInt(a[0]);
            while ((line = in.readLine()) != null) Run(line);
            in.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    static private void Run(String input){
        VaccineState initial = new VaccineState(input, "", null, '-');
        BFSolver solver = new BFSolver();
        solver.solve(initial);
    }
}

class VaccineState {
    String stack1;
    String stack2;
    private VaccineState previous;
    char move; //move done to get to that state

    public VaccineState(String s1, String s2, VaccineState p, char m) {
        stack1 = s1;
        stack2 = s2;
        previous = p;
        move = m;
    }

    public boolean Safe() {
        boolean[] check = new boolean[4];
        for (int i = 0; i < 4; i++) check[i] = false;
        char prev = '-';
        char c;
        for (int i = 0; i < stack2.length(); i++) {
            c = stack2.charAt(i);
            if (c == 'A') {
                if (!check[0]) check[0] = true;
                else {
                    if (prev != 'A') return false;
                }
            } else if (c == 'U') {
                if (!check[1]) check[1] = true;
                else {
                    if (prev != 'U') return false;
                }
            } else if (c == 'C') {
                if (!check[2]) check[2] = true;
                else {
                    if (prev != 'C') return false;
                }
            } else if (c == 'G') {
                if (!check[3]) check[3] = true;
                else {
                    if (prev != 'G') return false;
                }
            }
            prev = c;
        }
        return true;
    }

    public boolean isFinal() {
        return (stack1.equals("") && !stack2.equals("") && this.Safe());
    }

    public ArrayList<VaccineState> next() {
        ArrayList<VaccineState> nstates = new ArrayList<>();
        //insert with lexicographic order c,p,r
        //complement move first
        //Don't apply "complement move" if stack1 is empty
        /*Don't apply "complement move" to initial state
        because the problem that arises is complement
         */
        if (stack1 != "" && previous != null) {
            nstates.add(new VaccineState(complement(stack1), stack2, this, 'c'));
        }
        //push move second
        //Don't apply "push move" if stack1 is empty
        int len = stack1.length();
        if (len != 0) {
            char c = stack1.charAt(len-1);
            nstates.add(new VaccineState(stack1.substring(0,len-1), stack2.concat(String.valueOf(c)), this, 'p'));
        }
        //else nstates.add(new VaccineState(CopyOfStack(stack1), CopyOfStack(stack2), this, 'p'));
        //reverse move third
        if (!stack2.equals("")) {
            nstates.add(new VaccineState(stack1, reverse(stack2), this, 'r'));
        }
        return nstates;
    }

    private String complement(String s1) {
        char[] s2 = new char[s1.length()];
        for (int i = 0; i < s1.length(); i++) {
            if (s1.charAt(i) == 'A') s2[i] = 'U';
            else if (s1.charAt(i) == 'U') s2[i] = 'A';
            else if (s1.charAt(i) == 'G') s2[i] = 'C';
            else if (s1.charAt(i) == 'C') s2[i] = 'G';
        }
        return String.valueOf(s2);
    }

    private String reverse(String s1) {
        char[] s2 = new char[s1.length()];
        for (int i = s1.length() - 1; i >= 0; i--) {
            s2[(s1.length() - 1)-i] = s1.charAt(i);
        }
        return String.valueOf(s2);
    }

    public VaccineState getPrevious() {
        return this.previous;
    }

    @Override
    public boolean equals(Object o) {
        //System.out.println("Call equals!");
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        VaccineState other = (VaccineState) o;
        return (this.stack1.equals(other.stack1)) && (this.stack2.equals(other.stack2));
    }

    @Override
    public int hashCode() {
        //System.out.println("Call HashCode...");
        return Objects.hash(stack1, stack2);
    }

    public void PrintState(){
        System.out.print("State:[Stack1: ");
        if(stack1.equals("")) System.out.print("EMPTY");
        else System.out.print(stack1);
        System.out.print(", Stack2: ");
        if(stack2.equals("")) System.out.print("EMPTY");
        else System.out.print(stack2);
        System.out.print("]");
    }
}

class BFSolver {
    public void solve (VaccineState initial){
        Set<VaccineState> seen = new HashSet<>();
        Queue<VaccineState> queue = new ArrayDeque<>();
        queue.add(initial);
        seen.add(initial);
        while(!queue.isEmpty()){
            /*System.out.println("Queue..");
            for(VaccineState s: queue) {
                s.PrintState();
                System.out.print(", ");
            }
            System.out.println();
            System.out.println("Seen...");
            for(VaccineState s: seen) {
                s.PrintState();
                System.out.print(", ");
            }
            System.out.println();*/
            VaccineState s = queue.remove();
            if(s.isFinal()) {
                PrintSolution(s);
                break;
            }
            for(VaccineState n : s.next()) {
                if(!seen.contains(n)){
                    seen.add(n);
                    if(n.Safe()) queue.add(n);
                }
            }
        }
    }
    public void PrintSolution(VaccineState s){
        VaccineState p = s.getPrevious();
        if(p == null) System.out.println();
        else {
            PrintSolution(p);
            System.out.print(s.move);
        }
    }
}
