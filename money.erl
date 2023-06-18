-module(money).
-export([start/1]).
-import(customer, [spawn_customers/3]).
-import(bank, [spawn_banks/2]).

start(Args) ->
    CustomerFile = lists:nth(1, Args),
    io:format("CustomerFile: ~p~n", [CustomerFile]),
    BankFile = lists:nth(2, Args),
    {ok, CustomerInfo} = file:consult(CustomerFile),
    {ok, BankInfo} = file:consult(BankFile),
    CustomerNames = [Name || {Name, _} <- CustomerInfo],
    BankNames = [Name || {Name, _} <- BankInfo],
    MasterPid = self(),
    register(master, MasterPid),
    spawn_banks(BankInfo, MasterPid),
    spawn_customers(CustomerInfo, BankNames, MasterPid),
    loop(MasterPid, length(CustomerNames),0,[],BankInfo).

loop(MasterPid,CustomersListLength,CustomerTasksFinisedCounter,CustomerOutputList,BankInfo) ->
    if 
        CustomerTasksFinisedCounter =:= CustomersListLength ->
            io:format("** Banking Report ** ~n"),
            io:format("~n Customers: ~n"),
            print_list(CustomerOutputList,0,0),

            BankListUpdated = exitBankProcesses(BankInfo,[]),
            io:format("~n Banks: ~n"),
            print_Bank_list(BankListUpdated,0,0);
        true ->
            receive
                {bank_response, BankName,CustomerName,LoanRequested, approves} ->
                    io:format("$ The ~s bank approves a loan of ~p dollar(s) to ~s.~n", [BankName,LoanRequested,CustomerName]),
                    loop(MasterPid,CustomersListLength,CustomerTasksFinisedCounter,CustomerOutputList,BankInfo);
                {bank_response, BankName,CustomerName,LoanRequested, denies} ->
                    io:format("$ The ~s bank denies a loan of ~p dollar(s) to ~s.~n", [BankName,LoanRequested,CustomerName]),
                    loop(MasterPid,CustomersListLength,CustomerTasksFinisedCounter,CustomerOutputList,BankInfo);
                {customer_response,BankName, CustomerName, LoanRequested} ->
                    io:format("? ~s requests a loan of ~p dollar(s) from the ~s.~n", [CustomerName, LoanRequested,BankName]),
                    loop(MasterPid,CustomersListLength,CustomerTasksFinisedCounter,CustomerOutputList,BankInfo);
                {MasterPid,CustomerName,LoanAmount,LoanApprovedAmount ,finished} ->
                    NewCustomerTasksFinisedCounter = CustomerTasksFinisedCounter + 1,
                    NewCustomerOutputList = lists:append(CustomerOutputList,[{CustomerName,LoanAmount,LoanApprovedAmount}]),
                    loop(MasterPid,CustomersListLength,NewCustomerTasksFinisedCounter,NewCustomerOutputList,BankInfo)
            end
    end.

print_list([], _Objective, _Received) ->
    io:format(" ----- ~n"),
    io:format(" Total: objective ~p, received ~p. ~n", [_Objective, _Received]);
    % Base case: empty list, stop recursion

print_list([Head | Tail], Objective, Received) ->
    NewObjective = Objective + element(2,Head),
    NewRecieved = Received + element(3,Head),

    % Print the list element with the custom parameter
    io:format(" ~s: objective ~p, received ~p.~n", [element(1,Head), element(2,Head), element(3,Head)]),

    % Recursive call with the updated parameter and the remaining list
    print_list(Tail, NewObjective,NewRecieved).

print_Bank_list([], _Original, _Loaned) ->
    io:format(" ----- ~n"),
    io:format(" Total: original ~p, loaned ~p.~n", [_Original, _Loaned]);
    % Base case: empty list, stop recursion

print_Bank_list([Head | Tail], Original, Loaned) ->
    NewOriginal = Original + element(2,Head),
    NewLoaned = Loaned + element(3,Head),

    % Print the list element with the custom parameter
    io:format(" ~s: original ~p, balance ~p.~n", [element(1,Head), element(2,Head), element(3,Head)]),

    % Recursive call with the updated parameter and the remaining list
    print_Bank_list(Tail, NewOriginal, NewLoaned).

exitBankProcesses([], _BankList) ->
    _BankList;

exitBankProcesses([Head | Tail], BankList) -> 
    BankName = element(1,Head),
    BankBalance = element(2,Head),
    case whereis(BankName) of
        undefined ->
            io:format(BankName ," process is not registered.~n");
        Pid ->
            % Send a message to the Bank using its process ID
            Pid ! {self(), exit_signal},
            % Receive the result back
            receive
                {BankName,ApprovedLoan} ->
                    NewBankList = lists:append(BankList,[{BankName, BankBalance, ApprovedLoan}]),
                    exitBankProcesses(Tail, NewBankList)
            after
                5000 ->  % Timeout after 5 seconds
                    io:format("Did not receive a exit response from bank process.~n")
            end
    end.