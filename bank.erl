-module(bank).
-export([spawn_banks/2]).

spawn_banks(BankInfo, MasterPid) ->
    lists:foreach(fun({BankName, BankBalance}) -> 
        % Create Process of each Bank
        Pid = spawn(fun() -> bank(BankName,BankBalance,MasterPid) end),
        register(BankName, Pid)
    end, 
    BankInfo).

bank(BankName,BankBalance,MasterPid) ->
    receive
        {SenderPid, CustomerName, LoanRequested} ->
            if 
                BankBalance - LoanRequested >=0 ->
                    NewBalance = BankBalance - LoanRequested,
                    MasterPid ! {bank_response,BankName,CustomerName,LoanRequested,approves}, %Send logs to print in master
                    SenderPid ! {approves}, %Send signal to customer process
                    %send approves message to master process to print logs
                    bank(BankName,NewBalance,MasterPid);
                true -> 
                    %send deniel message to master process to print logs
                    MasterPid ! {bank_response,BankName,CustomerName,LoanRequested,denies}, %Send logs to print in master
                    SenderPid ! {denies}, %Send signal to customer process
                    bank(BankName,BankBalance,MasterPid)
            end;
        {SenderPid, exit_signal} ->
                    exit(SenderPid,normal),
                    SenderPid ! {BankName,BankBalance}
    end.