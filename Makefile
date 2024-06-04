# NOTE: Dependencies are intentionally not listed.

.PHONY: delete-golden
delete-golden: \
  delete-cabal-golden \
  delete-json-golden \
  delete-yaml-golden

.PHONY: delete-cabal-golden
delete-cabal-golden:
	cd test-suite-golden \
	&& find -type f \
	| grep --perl-regexp ".*\.cabal\.golden$$" \
	| xargs --no-run-if-empty --max-args=1 --verbose rm

.PHONY: delete-json-golden
delete-json-golden:
	cd test-suite-golden \
	&& find -type f \
	| grep --perl-regexp ".*\.json\.golden$$" \
	| xargs --no-run-if-empty --max-args=1 --verbose rm

.PHONY: delete-yaml-golden
delete-yaml-golden:
	cd test-suite-golden \
	&& find -type f \
	| grep --perl-regexp ".*\.yaml\.golden$$" \
	| xargs --no-run-if-empty --max-args=1 --verbose rm

.PHONY: delete-generated
.PHONY: delete-generated-cabal
.PHONY: delete-generated-json
.PHONY: delete-generated-yaml
delete-generated: \
  delete-generated-cabal \
  delete-generated-json \
  delete-generated-yaml

delete-generated-cabal:
	cd test-suite-golden \
	&& find -type f \
	| grep --perl-regexp ".*(?<!\.yaml)\.cabal$$" \
	| xargs --no-run-if-empty --max-args=1 --verbose rm

delete-generated-json:
	cd test-suite-golden \
	&& find -type f \
	| grep --perl-regexp ".*\.json$$" \
	| xargs --no-run-if-empty --max-args=1 --verbose rm

delete-generated-yaml:
	cd test-suite-golden \
	&& find -type f \
	| grep --perl-regexp ".*(?<!package)\.yaml$$" \
	| xargs --no-run-if-empty --max-args=1 --verbose rm

KY := test-suite-golden/test-files/key
RW := test-suite-golden/test-files/real-world

.PHONY: generate-cabal
generate-cabal: \
  $(KY)/empty-inferred/empty-inferred.cabal \
  $(KY)/empty-package.cabal \
  $(KY)/with-GHC2021.cabal \
  $(KY)/when-dependencies.cabal \
  $(KY)/import-relative/import-relative.cabal \
  $(KY)/import-local/import-local.cabal \
  $(KY)/empty-inferred/empty-inferred.cabal \
  $(RW)/stack/stack.cabal \
  $(RW)/hpack/hpack.cabal

$(KY)/empty-package.cabal:
	dhall-hpack-cabal --package-dhall=$(@:.cabal=.dhall)

$(KY)/when-dependencies.cabal:
	dhall-hpack-cabal --package-dhall=$(@:.cabal=.dhall)

$(KY)/with-GHC2021.cabal:
	dhall-hpack-cabal --package-dhall=$(@:.cabal=.dhall)

$(KY)/%.cabal:
	cd $(@D) && dhall-hpack-cabal

$(RW)/%.cabal:
	dhall-hpack-cabal --package-dhall=$(@:.cabal=.dhall)

.PHONY: generate-json
generate-json: \
  $(KY)/empty-inferred/package.json \
  $(KY)/empty-package.json \
  $(KY)/with-GHC2021.json \
  $(KY)/when-dependencies.json \
  $(KY)/import-relative/package.json \
  $(KY)/import-local/package.json \
  $(KY)/empty-inferred/package.json \
  $(RW)/stack/stack.json \
  $(RW)/hpack/hpack.json

$(KY)/empty-package.json:
	dhall-hpack-json --package-dhall=$(@:.json=.dhall) > $@

$(KY)/when-dependencies.json:
	dhall-hpack-json --package-dhall=$(@:.json=.dhall) > $@

$(KY)/with-GHC2021.json:
	dhall-hpack-json --package-dhall=$(@:.json=.dhall) > $@

$(KY)/%/package.json:
	cd $(@D) && dhall-hpack-json > $(@F)

$(RW)/%.json:
	dhall-hpack-json --package-dhall=$(@:.json=.dhall) > $@

.PHONY: generate-yaml
generate-yaml: \
  $(KY)/empty-inferred/package.yaml \
  $(KY)/empty-package.yaml \
  $(KY)/with-GHC2021.yaml \
  $(KY)/when-dependencies.yaml \
  $(KY)/import-relative/package.yaml \
  $(KY)/import-local/package.yaml \
  $(KY)/empty-inferred/package.yaml \
  $(RW)/stack/stack.yaml \
  $(RW)/hpack/hpack.yaml

$(KY)/empty-package.yaml:
	dhall-hpack-yaml --package-dhall=$(@:.yaml=.dhall) > $@

$(KY)/when-dependencies.yaml:
	dhall-hpack-yaml --package-dhall=$(@:.yaml=.dhall) > $@

$(KY)/with-GHC2021.yaml:
	dhall-hpack-yaml --package-dhall=$(@:.yaml=.dhall) > $@

$(KY)/%/package.yaml:
	cd $(@D) && dhall-hpack-yaml > $(@F)

$(RW)/%.yaml:
	dhall-hpack-yaml --package-dhall=$(@:.yaml=.dhall) > $@
