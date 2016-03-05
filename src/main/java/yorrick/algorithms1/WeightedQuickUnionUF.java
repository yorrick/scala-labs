package yorrick.algorithms1;

public class WeightedQuickUnionUF {
    private int[] parent;
    private int[] size;

    public WeightedQuickUnionUF(int N) {
        parent = new int[N];
        size = new int[N];

        for (int i = 0; i < N; i++) {
            parent[i] = i;
            size[i] = 1;
        }
    }
    
    private int root(int i) {
        while (i != parent[i]) {
            i = parent[i];
        }
        
        return i;
    }
    
    public boolean connected(int p, int q) {
        return root(p) == root(q);
    }
    
    public void union(int p, int q) {
        int proot = root(p);
        int qroot = root(q);

        if (proot == qroot) return;

        if (size[proot] < size[qroot]) {
            parent[proot] = qroot;
            size[qroot] += size[proot];
        }
        else {
            parent[qroot] = proot;
            size[proot] += size[qroot];
        }
    }
    
    public static void main(String[] args) {
        WeightedQuickUnionUF qf = new WeightedQuickUnionUF(10);
        qf.union(2,3);
        qf.union(2,7);
        qf.union(4,8);
        
        System.out.println(qf.connected(2, 7)); // true
        System.out.println(qf.connected(2, 8)); // false
    }
    
    
}
