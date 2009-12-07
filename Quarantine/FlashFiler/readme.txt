The following units are for the FlashFiler persistence layer contributed by
Carlo Marona. Below is snippets from Carlo's original email explaining what
is required to use the FlashFiler persistence layer.


---------------------[ start ]-------------------------
Date: Sun, 09 Nov 2008 16:35:05 +0100
From: Carlo Marona <c.marona@tiscali.it>

Hi,
I have some corrections, additions and suggestions.


-In tiConstants.pas I added those lines:
  cTIPersistEmbeddedFF   = 'EmbeddedFlashFiler';
  cTIPersistRemoteFF     = 'RemoteFlashFiler';

I only added the code for the last two but I haven't made tests, only 
compilation checked ok.

-In tiDefines.inc I added those lines:
{$IFDEF LINK_FLASHFILER_EMBEDDED}  {$DEFINE STATIC_PERLAYER_LINKING} {$ENDIF}
{$IFDEF LINK_FLASHFILER_REMOTE}    {$DEFINE STATIC_PERLAYER_LINKING} {$ENDIF}

-In tiOPFManager.pas in the implementation uses clause
  {$IFDEF LINK_FLASHFILER_EMBEDDED}  ,tiQueryEmbeddedFalshFiler  {$ENDIF}
  {$IFDEF LINK_FLASHFILER_REMOTE}    ,tiQueryRemoteFlashFiler  {$ENDIF}

-In tiOPFManager.pas in the initialization section I added
  {$IFDEF LINK_FLASHFILER_EMBEDDED} GTIOPFManager.DefaultPersistenceLayerName := cTIPersistEmbeddedFF; {$ENDIF}
  {$IFDEF LINK_FLASHFILER_REMOTE} GTIOPFManager.DefaultPersistenceLayerName := cTIPersistRemoteFF; {$ENDIF}


Cheers.
  Carlo Marona

----------------------[ end ]-------------------------

