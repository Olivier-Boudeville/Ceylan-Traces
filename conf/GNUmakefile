TRACES_TOP = ..


.PHONY: clean-local clean-rebar


# Typically to generate various flavours (normal, for Hex, for testing, etc.) of
# rebar.config files:
#
# (note: not centralised in a GNUmakerules-automatic file for all layers, as
# substitutions are layer-specific)
#
%: %.template
	@echo "  Generating $@ from $< (VERSION_FOR_REBAR3 being $(VERSION_FOR_REBAR3), WOOPER_VERSION_FOR_REBAR being $(WOOPER_VERSION), MYRIAD_VERSION_FOR_REBAR being $(MYRIAD_VERSION))"
	@cat $< | sed "s|\"MYRIAD_VERSION_FOR_REBAR\"|\"$(MYRIAD_VERSION)\"|g" | sed "s|\"WOOPER_VERSION_FOR_REBAR\"|\"$(WOOPER_VERSION)\"|g" | sed "s|\"VERSION_FOR_REBAR3\"|\"$(VERSION_FOR_REBAR3)\"|g" > $@


clean: clean-local


clean-local: clean-rebar
	-@/bin/rm -f $(REBAR_CONF_FILES)


clean-rebar:
	-@/bin/rm -f rebar.config
	-@/bin/rm -rf priv/


include $(TRACES_TOP)/GNUmakesettings.inc
