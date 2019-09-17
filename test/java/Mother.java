// TODO: this should be a test resource
public class Mother {
  static class Infant {}
  interface Crying {}

  static class Child extends Infant implements Crying
  {
    static class Toy {}
  }

  public Child giveBirth() {
    return new Child();
  }
}
