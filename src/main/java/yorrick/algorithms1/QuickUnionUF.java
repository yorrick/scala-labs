package yorrick.algorithms1;

public class QuickUnionUF {
    private int[] id;

    public QuickUnionUF(int N) {
        id = new int[N];
        
        for (int i = 0; i < N; i++) {
            id[i] = i;
        }
    }
    
    private int root(int i) {
        while (i != id[i]) {
            i = id[i];
        }
        
        return i;
    }
    
    public boolean connected(int p, int q) {
        return root(p) == root(q);
    }
    
    public void union(int p, int q) {
        int proot = root(p);
        int qroot = root(q);
        id[proot] = qroot;
    }
    
    public static void main(String[] args) {
        QuickUnionUF qf = new QuickUnionUF(10);
        qf.union(2,3);
        qf.union(2,7);
        qf.union(4,8);
        
        System.out.println(qf.connected(2, 7));
        System.out.println(qf.connected(2, 8));
    }
    
    
}
