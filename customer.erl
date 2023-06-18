-module(customer).
-export([spawn_customers/3]).

spawn_customers(CustomerInfo, BankNames, MasterPid) ->
    lists:foreach(
        fun({CustomerName, LoanAmount}) ->
            Pid = spawn(fun() -> 
                timer:sleep(200),  % Sleep for 200 milliseconds
                customer(CustomerName,LoanAmount,0, BankNames, MasterPid) 
            end),
            register(CustomerName, Pid)
        end,
        CustomerInfo
    ).

customer(CustomerName,LoanAmount,RecievedAmount, BankNames, MasterPid) ->
    if 
        LoanAmount =:= RecievedAmount ->
            exit(MasterPid, normal),
            MasterPid ! {MasterPid,CustomerName,LoanAmount,RecievedAmount,finished}; %Send logs to print in master
        true ->
            case BankNames of
                [] ->
                    exit(MasterPid, normal),
                    MasterPid ! {MasterPid,CustomerName,LoanAmount,RecievedAmount,finished}; %Send logs to print in master
                _ ->
                    % List is not empty, continue processing
                    RandomIndex = rand:uniform(length(BankNames)),
                    RandomBankName = lists:nth(RandomIndex, BankNames),

                    case whereis(RandomBankName) of
                        undefined ->
                            io:format(RandomBankName ," process is not registered.~n");
                        Pid ->
                            % Send a message to the Bank using its process ID
                            RandomAmount = rand:uniform(50),
                            RandomDelay = rand:uniform(91) + 10,  % Generate a random number between 0 and 90, then add 10
                            timer:sleep(RandomDelay),  % Sleep for the randomly generated delay in milliseconds
                            if 
                                RandomAmount =< LoanAmount - RecievedAmount ->
                                    Pid ! {self(),CustomerName, RandomAmount},
                                    MasterPid ! {customer_response,RandomBankName,CustomerName,RandomAmount}, %Send logs to print in master
                                    % Receive the result back
                                    receive
                                        {Result} ->
                                            if 
                                                Result == approves ->
                                                    NewRecievedAmount = RecievedAmount + RandomAmount,
                                                    customer(CustomerName,LoanAmount,NewRecievedAmount, BankNames, MasterPid);
                                                Result == denies ->
                                                    UpdatedBankNamesList = lists:delete(RandomBankName, BankNames),
                                                    customer(CustomerName,LoanAmount,RecievedAmount, UpdatedBankNamesList, MasterPid);
                                                true ->
                                                % Default case
                                                customer(CustomerName,LoanAmount,RecievedAmount, BankNames, MasterPid)
                                            end
                                    after
                                        5000 ->  % Timeout after 5 seconds
                                            io:format("Did not receive a response by customer.~n")
                                    end;
                                true ->
                                    customer(CustomerName,LoanAmount,RecievedAmount, BankNames, MasterPid)
                            end
                    end     
            end
    end.