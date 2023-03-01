namespace HelloWorld
{
    class Test
    {
        int A;
        int B;

        public Test(int a)
        {
            A = a;
            B = 3;
        }

        public Test(int a, int b)
        {
            A = a;
            B = b;
        }

        public string GetValue(string test) => test;
        public int GetValue() => A + B;

        public int TestDefault(int value = 9) => value;
    }

    class Program
    {
        static void Print(object message)
        {
# luau
print(message)
# endluau
        }

        static int Main()
        {
            Test test = new Test(4, 5);

            Print(test.GetValue());
            Print(test.TestDefault(12));
            Print(test.TestDefault());

            return 0;
        }
    }
}