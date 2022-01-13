This is a work-in-progress Salesforce.com backup web application implemented with [Yesod](https://hackage.haskell.org/package/yesod).

Here is an overview of the directories in this repository:

* ``cassava``: A local version of [``cassava``](https://hackage.haskell.org/package/cassava) that is extended to support [``TreeBuilder``s](https://hackage.haskell.org/package/bytestring-tree-builder-0.2.7.9/docs/ByteString-TreeBuilder.html).
* ``hs-certificate``: A local version of [X.509 certificate handling libraries](https://github.com/vincenthz/hs-certificate) with fixes for bugs related to ASN1 encoding of certificates.
* ``lib``: A library for interfacing with the Salesforce.com APIs.
* ``web-app``: A web application for controlling Salesforce.com backup jobs.
* ``yesod-form-bootstrap4``: A local version of [``yesod-form-bootstrap4``](https://hackage.haskell.org/package/yesod-form-bootstrap4) with some changes to render forms better.
