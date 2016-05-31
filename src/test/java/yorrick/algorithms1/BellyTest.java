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

    @Test
    public void testString() {
        System.out.println("toto\ntata\r".replaceAll("[\n\r]+", ""));
        //assertEquals("titi\\u000D".replaceAll("\\r\\n|[\\n\\x0013\\x0B\\x0C\\r\\u0085\\u2028\\u2029]", ""), "titi");

        int c = 0x000D;
        String s = Character.toString((char)c);
        System.out.println(s);
        System.out.println(("titi" + s + "tata").indexOf(s));

        assertEquals(("titi" + s + "tata").replaceAll(s, ""), "tititata");
        assertEquals(("titi" + s + "tata").replaceAll(String.format("[\n%s]+", s), ""), "tititata");

        //assertEquals("toto\ntata\rtiti\\u000D".replaceAll("[\n\r\\u000D]+", ""), "tototatatiti");
    }
}