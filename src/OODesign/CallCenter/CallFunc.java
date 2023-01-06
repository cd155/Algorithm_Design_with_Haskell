package CallCenter;
import java.util.*;

public class CallFunc {
    public static void dispatchCall(List<Employee> respondents)
    {

    };

    private static Employee getFirstResp(List<Employee> respondents)
    {
        Employee rep = null; 
        for (Employee emp: respondents)
        {
            if (emp.isAvailable){
                rep = emp;
            }
        }
        return rep;
    };
}
