alias reload!='. ~/.zshrc'
alias ll='ls -al --color=auto'
alias sagi='sudo apt-get install'
alias sasearch='sudo apt-cache search'
alias psgrep='ps -ef | grep -v grep | grep '
alias e='emacsclient -t'
alias ec='emacsclient -c'
alias vim='emacsclient -t'
alias vi='emacsclient -t'
alias k='gitk --all &'
alias vssh="vagrant ssh"
alias lmp-verify="mvn verify -P local-enterprise-test-db -am -DskipPreTestTeardown=false -DskipPostTestTeardown=true -DskipDesignTests=false -DskipTransactionTests=false -DskipMulticastTests=false -DskipTestCoverage=false -Dtoybox.limiting_service_host=http://127.0.0.1:9091/ -Dtoybox.code_manager_host=http://127.0.0.1:9091/ -pl ControlCenter"
alias lmp-deploy="mvn clean deploy -Dbuild.number=69 -P local-enterprise-db -DskipPreTestTeardown=true -DskipPostTestTeardown=true -DskipITs=true -DskipMulticastTests=true -DskipTestCoverage=true -DdeployUrl=scp://vagrant:vagrant@lmp-local:2200/opt/LMP/lib"
