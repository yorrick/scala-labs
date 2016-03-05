package yorrick.algorithms1;

import org.junit.Test;
import org.junit.runner.RunWith;

import static org.junit.Assert.assertEquals;
import org.junit.runners.BlockJUnit4ClassRunner;

@RunWith(BlockJUnit4ClassRunner.class)
public class BellyTest {
    @Test
    public void testEat() {
        Belly b = new Belly();
        assertEquals(1, b.eat(1));
    }
}