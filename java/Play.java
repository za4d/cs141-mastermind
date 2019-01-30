import java.util.Scanner;

public class Play {
  private static Scanner input = new Scanner(System.in);

  public static void main(String[] args) {
    Game mm = new Game();
    System.out.println("Mode? \n1:Manual  2:Auto");
    int mode = input.nextInt();
    if (mode == 1) {
      System.out.println("manual");
    } else {
      System.out.println("auto");
    }
  }
