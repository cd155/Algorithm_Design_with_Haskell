package CallCenter;

// 7.2
// Imagine you have a call center with three levels of employees: 
// respondent, manager, and director. An incoming telephone call must be 
// first allocated to a respondent who is free. If the respondent can't 
// handle the call, he or she must escalate the call to a manager. If the 
// manager is not free or not able to handle it, then the call should be 
// escalated to a director. Design the classes and data structures for 
// this problem. Implement a method dispatchCall() which assigns a call 
// to the first available employee.

// class employee: 
//     property:
//         levelType
//         isAvailable
//         nextLevel
//         callNumber
    
//     function:
//         signCall()
//         completeCall()


// static method:
//     dispatchCall([respondent], call)
//         1. find the first available respondent, sign a call
//             or 2. find the first available manager, sign a call
//                 or 3. find the first available director, sign a call
    
//     getFirstResp([respondent]) -> a list of employee

//     getFirstManager([respondent]) -> a list of employee

//     getFirstDirector([respondent]) -> a list of employee

//     check Resp, manage, director, to find the first one

public class Employee
{
    private String levelType;
    
    public Employee supervisor;
    public String callNumber;
    public Boolean isAvailable;
    public String name;
    
    public Employee()
    {
        levelType = "";
        isAvailable = false;
        supervisor = null;
        callNumber = "";
    }

    public void signedCall(String callNumber)
    {
        this.callNumber = callNumber;
        isAvailable = false;
    }

    public void completeCall()
    {
        this.callNumber = null;
        isAvailable = false;
    }
}
