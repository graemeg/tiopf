This is code contributed by Andrew Denton. These are units used to
generate listing reports with grouping support.  It was originally
written for tiOPF1 and Report Builder. Since then he has switched
to Fast Report, but with the same reporting interface.

For more details on what this is and how it works, take a look at the
message thread in the tiopf.support newsgroup where he explains it:

   Subject: Anyone else using Report Builder?
   From: Andrew Denton
   Date: 2002-05-03
   Message-ID: <49fd.428b33d7.c92b3@goofy.opensoft.homeip.net>
   Lines: 59
   Bytes: 2092
   
   
  ---------------------------------------------

From: Andrew Denton <andy@nospam.romar.co.uk>
Newsgroups: tiopf.support
Subject: Re: Anyone else using Report Builder?
Date: Thu, 2 Jun 2011 09:35:31 +0100
Message-ID: <MPG.28515ba9f1f84eea989683@opensoft.homeip.net>
Bytes: 1781
Lines: 53
Xref: goofy.opensoft.homeip.net tiopf.support:7405

Hi Graeme,

I added it to the repository for tiOPF3/Quarantine/ReportUtils. There 
are 3 files:-
  ReportUtils - (the original Report Builder classes)
  FastReportUtils - (the later version for Fast Reports and what I use).
  ELVReportUtils - A descendent class for report generation from an EasyListView
    
They should work unmodified for tiOPF3 and  could also be back-ported 
to tiOPF1 if the need arose (the concept was originally created for 
tiOPF1). 
     
Best regards,
     
Andy
     
  -----------------------------------------------
 
     