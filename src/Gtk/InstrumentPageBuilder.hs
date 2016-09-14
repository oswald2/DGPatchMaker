module Gtk.InstrumentPageBuilder
(builderFileAsString)
where


builderFileAsString :: String
builderFileAsString = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!-- Generated with glade 3.16.1 -->\n<interface>\n  <requires lib=\"gtk+\" version=\"3.10\"/>\n  <object class=\"GtkAdjustment\" id=\"adjustmentAttack\">\n    <property name=\"lower\">10</property>\n    <property name=\"upper\">1000</property>\n    <property name=\"value\">300</property>\n    <property name=\"step_increment\">1</property>\n    <property name=\"page_increment\">100</property>\n  </object>\n  <object class=\"GtkAdjustment\" id=\"adjustmentNrHits\">\n    <property name=\"lower\">1</property>\n    <property name=\"upper\">100</property>\n    <property name=\"value\">1</property>\n    <property name=\"step_increment\">1</property>\n    <property name=\"page_increment\">10</property>\n  </object>\n  <object class=\"GtkDialog\" id=\"dialogNrHits\">\n    <property name=\"can_focus\">False</property>\n    <property name=\"type\">popup</property>\n    <property name=\"window_position\">mouse</property>\n    <property name=\"type_hint\">dialog</property>\n    <child internal-child=\"vbox\">\n      <object class=\"GtkBox\" id=\"dialog-vbox1\">\n        <property name=\"can_focus\">False</property>\n        <property name=\"orientation\">vertical</property>\n        <property name=\"spacing\">2</property>\n        <child internal-child=\"action_area\">\n          <object class=\"GtkButtonBox\" id=\"dialog-action_area1\">\n            <property name=\"can_focus\">False</property>\n            <property name=\"layout_style\">end</property>\n            <child>\n              <placeholder/>\n            </child>\n            <child>\n              <placeholder/>\n            </child>\n          </object>\n          <packing>\n            <property name=\"expand\">False</property>\n            <property name=\"fill\">True</property>\n            <property name=\"pack_type\">end</property>\n            <property name=\"position\">0</property>\n          </packing>\n        </child>\n        <child>\n          <object class=\"GtkBox\" id=\"box7\">\n            <property name=\"visible\">True</property>\n            <property name=\"can_focus\">False</property>\n            <child>\n              <object class=\"GtkLabel\" id=\"label8\">\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">False</property>\n                <property name=\"xpad\">10</property>\n                <property name=\"label\" translatable=\"yes\">Nr of Hits:</property>\n              </object>\n              <packing>\n                <property name=\"expand\">False</property>\n                <property name=\"fill\">True</property>\n                <property name=\"position\">0</property>\n              </packing>\n            </child>\n            <child>\n              <object class=\"GtkSpinButton\" id=\"spinbuttonNrHits\">\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">True</property>\n                <property name=\"input_purpose\">digits</property>\n                <property name=\"adjustment\">adjustmentNrHits</property>\n                <property name=\"value\">1</property>\n              </object>\n              <packing>\n                <property name=\"expand\">True</property>\n                <property name=\"fill\">True</property>\n                <property name=\"position\">1</property>\n              </packing>\n            </child>\n          </object>\n          <packing>\n            <property name=\"expand\">False</property>\n            <property name=\"fill\">True</property>\n            <property name=\"position\">1</property>\n          </packing>\n        </child>\n      </object>\n    </child>\n  </object>\n  <object class=\"GtkAdjustment\" id=\"adjustmentSpread\">\n    <property name=\"lower\">1</property>\n    <property name=\"upper\">2000</property>\n    <property name=\"value\">1000</property>\n    <property name=\"step_increment\">1</property>\n    <property name=\"page_increment\">100</property>\n  </object>\n  <object class=\"GtkBox\" id=\"mainBox\">\n    <property name=\"visible\">True</property>\n    <property name=\"can_focus\">False</property>\n    <property name=\"orientation\">vertical</property>\n    <child>\n      <object class=\"GtkBox\" id=\"box3\">\n        <property name=\"visible\">True</property>\n        <property name=\"can_focus\">False</property>\n        <property name=\"orientation\">vertical</property>\n        <child>\n          <object class=\"GtkBox\" id=\"box2\">\n            <property name=\"visible\">True</property>\n            <property name=\"can_focus\">False</property>\n            <child>\n              <object class=\"GtkButton\" id=\"buttonImportInstrument\">\n                <property name=\"label\" translatable=\"yes\">Import DrumDrops Instrument...</property>\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">True</property>\n                <property name=\"receives_default\">True</property>\n                <property name=\"xalign\">0.57999998331069946</property>\n              </object>\n              <packing>\n                <property name=\"expand\">False</property>\n                <property name=\"fill\">True</property>\n                <property name=\"position\">0</property>\n              </packing>\n            </child>\n            <child>\n              <object class=\"GtkButton\" id=\"buttonLoadInstrument\">\n                <property name=\"label\" translatable=\"yes\">Load Instrument...</property>\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">True</property>\n                <property name=\"receives_default\">True</property>\n                <property name=\"yalign\">0.46000000834465027</property>\n              </object>\n              <packing>\n                <property name=\"expand\">False</property>\n                <property name=\"fill\">True</property>\n                <property name=\"position\">1</property>\n              </packing>\n            </child>\n            <child>\n              <object class=\"GtkButton\" id=\"buttonExportInstrument\">\n                <property name=\"label\" translatable=\"yes\">Export Instrument</property>\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">True</property>\n                <property name=\"receives_default\">True</property>\n              </object>\n              <packing>\n                <property name=\"expand\">False</property>\n                <property name=\"fill\">True</property>\n                <property name=\"pack_type\">end</property>\n                <property name=\"position\">2</property>\n              </packing>\n            </child>\n          </object>\n          <packing>\n            <property name=\"expand\">False</property>\n            <property name=\"fill\">True</property>\n            <property name=\"padding\">5</property>\n            <property name=\"position\">0</property>\n          </packing>\n        </child>\n        <child>\n          <object class=\"GtkBox\" id=\"box5\">\n            <property name=\"visible\">True</property>\n            <property name=\"can_focus\">False</property>\n            <child>\n              <object class=\"GtkButton\" id=\"buttonCalcHits\">\n                <property name=\"label\" translatable=\"yes\">Caclulate Hit Power</property>\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">True</property>\n                <property name=\"receives_default\">True</property>\n                <property name=\"xalign\">0.52999997138977051</property>\n              </object>\n              <packing>\n                <property name=\"expand\">False</property>\n                <property name=\"fill\">True</property>\n                <property name=\"padding\">10</property>\n                <property name=\"pack_type\">end</property>\n                <property name=\"position\">0</property>\n              </packing>\n            </child>\n            <child>\n              <object class=\"GtkSpinButton\" id=\"spinbuttonSpread\">\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">True</property>\n                <property name=\"max_length\">4</property>\n                <property name=\"width_chars\">5</property>\n                <property name=\"input_purpose\">digits</property>\n                <property name=\"orientation\">vertical</property>\n                <property name=\"adjustment\">adjustmentSpread</property>\n                <property name=\"numeric\">True</property>\n                <property name=\"value\">1000</property>\n              </object>\n              <packing>\n                <property name=\"expand\">False</property>\n                <property name=\"fill\">True</property>\n                <property name=\"pack_type\">end</property>\n                <property name=\"position\">1</property>\n              </packing>\n            </child>\n            <child>\n              <object class=\"GtkLabel\" id=\"label7\">\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">False</property>\n                <property name=\"xpad\">10</property>\n                <property name=\"label\" translatable=\"yes\">Spread:</property>\n              </object>\n              <packing>\n                <property name=\"expand\">False</property>\n                <property name=\"fill\">True</property>\n                <property name=\"pack_type\">end</property>\n                <property name=\"position\">2</property>\n              </packing>\n            </child>\n            <child>\n              <object class=\"GtkSpinButton\" id=\"spinbuttonAttack\">\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">True</property>\n                <property name=\"max_length\">5</property>\n                <property name=\"width_chars\">5</property>\n                <property name=\"primary_icon_tooltip_text\" translatable=\"yes\">Selects how many samles should be considered for the attack</property>\n                <property name=\"input_purpose\">digits</property>\n                <property name=\"orientation\">vertical</property>\n                <property name=\"adjustment\">adjustmentAttack</property>\n                <property name=\"numeric\">True</property>\n                <property name=\"value\">300</property>\n              </object>\n              <packing>\n                <property name=\"expand\">False</property>\n                <property name=\"fill\">True</property>\n                <property name=\"pack_type\">end</property>\n                <property name=\"position\">3</property>\n              </packing>\n            </child>\n            <child>\n              <object class=\"GtkLabel\" id=\"label2\">\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">False</property>\n                <property name=\"xpad\">9</property>\n                <property name=\"label\" translatable=\"yes\">Attack:</property>\n              </object>\n              <packing>\n                <property name=\"expand\">False</property>\n                <property name=\"fill\">True</property>\n                <property name=\"pack_type\">end</property>\n                <property name=\"position\">4</property>\n              </packing>\n            </child>\n          </object>\n          <packing>\n            <property name=\"expand\">False</property>\n            <property name=\"fill\">True</property>\n            <property name=\"position\">1</property>\n          </packing>\n        </child>\n        <child>\n          <object class=\"GtkBox\" id=\"box4\">\n            <property name=\"visible\">True</property>\n            <property name=\"can_focus\">False</property>\n            <child>\n              <object class=\"GtkLabel\" id=\"label3\">\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">False</property>\n                <property name=\"xpad\">5</property>\n                <property name=\"label\" translatable=\"yes\">Version:</property>\n              </object>\n              <packing>\n                <property name=\"expand\">False</property>\n                <property name=\"fill\">True</property>\n                <property name=\"position\">0</property>\n              </packing>\n            </child>\n            <child>\n              <object class=\"GtkEntry\" id=\"entryVersion\">\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">True</property>\n                <property name=\"editable\">False</property>\n                <property name=\"width_chars\">5</property>\n              </object>\n              <packing>\n                <property name=\"expand\">False</property>\n                <property name=\"fill\">True</property>\n                <property name=\"position\">1</property>\n              </packing>\n            </child>\n            <child>\n              <object class=\"GtkLabel\" id=\"label4\">\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">False</property>\n                <property name=\"xpad\">5</property>\n                <property name=\"label\" translatable=\"yes\">Name:</property>\n              </object>\n              <packing>\n                <property name=\"expand\">False</property>\n                <property name=\"fill\">True</property>\n                <property name=\"position\">2</property>\n              </packing>\n            </child>\n            <child>\n              <object class=\"GtkEntry\" id=\"entryName\">\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">True</property>\n              </object>\n              <packing>\n                <property name=\"expand\">False</property>\n                <property name=\"fill\">True</property>\n                <property name=\"position\">3</property>\n              </packing>\n            </child>\n            <child>\n              <object class=\"GtkLabel\" id=\"label5\">\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">False</property>\n                <property name=\"xpad\">5</property>\n                <property name=\"label\" translatable=\"yes\">Type:</property>\n              </object>\n              <packing>\n                <property name=\"expand\">False</property>\n                <property name=\"fill\">True</property>\n                <property name=\"position\">4</property>\n              </packing>\n            </child>\n            <child>\n              <object class=\"GtkEntry\" id=\"entryType\">\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">True</property>\n                <property name=\"primary_icon_tooltip_text\" translatable=\"yes\">Specifies the instrument type. This helps the editor to automatically set some values as well as groups (e.g. Hi Hat). Valid types are: Kick, Snare, HiHat, Tom (RackTom &lt;n&gt;), Tom (Floor &lt;n&gt;), Cymbal, Ride, Shaker, Tambourine.</property>\n              </object>\n              <packing>\n                <property name=\"expand\">False</property>\n                <property name=\"fill\">True</property>\n                <property name=\"position\">5</property>\n              </packing>\n            </child>\n            <child>\n              <object class=\"GtkLabel\" id=\"label10\">\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">False</property>\n                <property name=\"xpad\">10</property>\n                <property name=\"label\" translatable=\"yes\">Filename:</property>\n              </object>\n              <packing>\n                <property name=\"expand\">False</property>\n                <property name=\"fill\">True</property>\n                <property name=\"position\">6</property>\n              </packing>\n            </child>\n            <child>\n              <object class=\"GtkEntry\" id=\"entryFileName\">\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">True</property>\n              </object>\n              <packing>\n                <property name=\"expand\">False</property>\n                <property name=\"fill\">True</property>\n                <property name=\"position\">7</property>\n              </packing>\n            </child>\n            <child>\n              <object class=\"GtkLabel\" id=\"label9\">\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">False</property>\n                <property name=\"xpad\">10</property>\n                <property name=\"label\" translatable=\"yes\">Change channel for selected to:</property>\n              </object>\n              <packing>\n                <property name=\"expand\">False</property>\n                <property name=\"fill\">True</property>\n                <property name=\"position\">8</property>\n              </packing>\n            </child>\n            <child>\n              <object class=\"GtkComboBox\" id=\"comboboxChannel\">\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">False</property>\n                <property name=\"has_entry\">True</property>\n                <child internal-child=\"entry\">\n                  <object class=\"GtkEntry\" id=\"combobox-entry\">\n                    <property name=\"can_focus\">False</property>\n                  </object>\n                </child>\n              </object>\n              <packing>\n                <property name=\"expand\">False</property>\n                <property name=\"fill\">True</property>\n                <property name=\"position\">9</property>\n              </packing>\n            </child>\n          </object>\n          <packing>\n            <property name=\"expand\">False</property>\n            <property name=\"fill\">True</property>\n            <property name=\"padding\">5</property>\n            <property name=\"position\">2</property>\n          </packing>\n        </child>\n      </object>\n      <packing>\n        <property name=\"expand\">False</property>\n        <property name=\"fill\">True</property>\n        <property name=\"position\">0</property>\n      </packing>\n    </child>\n    <child>\n      <object class=\"GtkBox\" id=\"box6\">\n        <property name=\"visible\">True</property>\n        <property name=\"can_focus\">False</property>\n        <child>\n          <object class=\"GtkPaned\" id=\"paned1\">\n            <property name=\"visible\">True</property>\n            <property name=\"can_focus\">True</property>\n            <property name=\"position\">320</property>\n            <child>\n              <object class=\"GtkBox\" id=\"box10\">\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">False</property>\n                <property name=\"orientation\">vertical</property>\n                <child>\n                  <object class=\"GtkFrame\" id=\"frame1\">\n                    <property name=\"visible\">True</property>\n                    <property name=\"can_focus\">False</property>\n                    <property name=\"label_xalign\">0</property>\n                    <property name=\"shadow_type\">none</property>\n                    <child>\n                      <object class=\"GtkAlignment\" id=\"alignment1\">\n                        <property name=\"visible\">True</property>\n                        <property name=\"can_focus\">False</property>\n                        <property name=\"left_padding\">12</property>\n                        <child>\n                          <object class=\"GtkScrolledWindow\" id=\"scrolledwindow2\">\n                            <property name=\"width_request\">50</property>\n                            <property name=\"visible\">True</property>\n                            <property name=\"can_focus\">True</property>\n                            <property name=\"shadow_type\">in</property>\n                            <child>\n                              <object class=\"GtkTreeView\" id=\"treeviewHit\">\n                                <property name=\"visible\">True</property>\n                                <property name=\"can_focus\">True</property>\n                                <property name=\"reorderable\">True</property>\n                                <property name=\"rules_hint\">True</property>\n                                <property name=\"search_column\">3</property>\n                                <property name=\"show_expanders\">False</property>\n                                <property name=\"enable_grid_lines\">horizontal</property>\n                                <property name=\"activate_on_single_click\">True</property>\n                                <child internal-child=\"selection\">\n                                  <object class=\"GtkTreeSelection\" id=\"treeview-selection1\"/>\n                                </child>\n                              </object>\n                            </child>\n                          </object>\n                        </child>\n                      </object>\n                    </child>\n                    <child type=\"label\">\n                      <object class=\"GtkLabel\" id=\"label6\">\n                        <property name=\"visible\">True</property>\n                        <property name=\"can_focus\">False</property>\n                        <property name=\"label\" translatable=\"yes\">Hits:</property>\n                      </object>\n                    </child>\n                  </object>\n                  <packing>\n                    <property name=\"expand\">True</property>\n                    <property name=\"fill\">True</property>\n                    <property name=\"position\">2</property>\n                  </packing>\n                </child>\n              </object>\n              <packing>\n                <property name=\"resize\">False</property>\n                <property name=\"shrink\">True</property>\n              </packing>\n            </child>\n            <child>\n              <object class=\"GtkBox\" id=\"box9\">\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">False</property>\n                <property name=\"orientation\">vertical</property>\n                <child>\n                  <object class=\"GtkFrame\" id=\"frame2\">\n                    <property name=\"visible\">True</property>\n                    <property name=\"can_focus\">False</property>\n                    <property name=\"label_xalign\">0</property>\n                    <property name=\"shadow_type\">none</property>\n                    <child>\n                      <object class=\"GtkAlignment\" id=\"alignment2\">\n                        <property name=\"visible\">True</property>\n                        <property name=\"can_focus\">False</property>\n                        <property name=\"left_padding\">12</property>\n                        <child>\n                          <object class=\"GtkScrolledWindow\" id=\"scrolledwindow1\">\n                            <property name=\"visible\">True</property>\n                            <property name=\"can_focus\">True</property>\n                            <property name=\"shadow_type\">in</property>\n                            <child>\n                              <object class=\"GtkTreeView\" id=\"treeviewSamples\">\n                                <property name=\"visible\">True</property>\n                                <property name=\"can_focus\">True</property>\n                                <property name=\"rules_hint\">True</property>\n                                <property name=\"show_expanders\">False</property>\n                                <property name=\"enable_grid_lines\">horizontal</property>\n                                <property name=\"enable_tree_lines\">True</property>\n                                <child internal-child=\"selection\">\n                                  <object class=\"GtkTreeSelection\" id=\"treeview-selection\"/>\n                                </child>\n                              </object>\n                            </child>\n                          </object>\n                        </child>\n                      </object>\n                    </child>\n                    <child type=\"label\">\n                      <object class=\"GtkLabel\" id=\"label1\">\n                        <property name=\"visible\">True</property>\n                        <property name=\"can_focus\">False</property>\n                        <property name=\"label\" translatable=\"yes\">Samples:</property>\n                      </object>\n                    </child>\n                  </object>\n                  <packing>\n                    <property name=\"expand\">True</property>\n                    <property name=\"fill\">True</property>\n                    <property name=\"position\">2</property>\n                  </packing>\n                </child>\n              </object>\n              <packing>\n                <property name=\"resize\">True</property>\n                <property name=\"shrink\">True</property>\n              </packing>\n            </child>\n          </object>\n          <packing>\n            <property name=\"expand\">True</property>\n            <property name=\"fill\">True</property>\n            <property name=\"position\">2</property>\n          </packing>\n        </child>\n      </object>\n      <packing>\n        <property name=\"expand\">True</property>\n        <property name=\"fill\">True</property>\n        <property name=\"position\">1</property>\n      </packing>\n    </child>\n  </object>\n  <object class=\"GtkAdjustment\" id=\"adjustmentStart\">\n    <property name=\"upper\">1000</property>\n    <property name=\"value\">1</property>\n    <property name=\"step_increment\">1</property>\n    <property name=\"page_increment\">10</property>\n  </object>\n  <object class=\"GtkAdjustment\" id=\"adjustmentStep\">\n    <property name=\"lower\">1</property>\n    <property name=\"upper\">1000</property>\n    <property name=\"value\">1</property>\n    <property name=\"step_increment\">1</property>\n    <property name=\"page_increment\">10</property>\n  </object>\n  <object class=\"GtkAdjustment\" id=\"adjustmentStop\">\n    <property name=\"lower\">1</property>\n    <property name=\"upper\">1000</property>\n    <property name=\"value\">127</property>\n    <property name=\"step_increment\">1</property>\n    <property name=\"page_increment\">10</property>\n  </object>\n  <object class=\"GtkDialog\" id=\"dialogHitPowerLin\">\n    <property name=\"can_focus\">False</property>\n    <property name=\"type_hint\">dialog</property>\n    <child internal-child=\"vbox\">\n      <object class=\"GtkBox\" id=\"dialog-vbox2\">\n        <property name=\"can_focus\">False</property>\n        <property name=\"orientation\">vertical</property>\n        <property name=\"spacing\">2</property>\n        <child internal-child=\"action_area\">\n          <object class=\"GtkButtonBox\" id=\"dialog-action_area2\">\n            <property name=\"can_focus\">False</property>\n            <property name=\"layout_style\">end</property>\n            <child>\n              <placeholder/>\n            </child>\n            <child>\n              <placeholder/>\n            </child>\n          </object>\n          <packing>\n            <property name=\"expand\">False</property>\n            <property name=\"fill\">True</property>\n            <property name=\"pack_type\">end</property>\n            <property name=\"position\">0</property>\n          </packing>\n        </child>\n        <child>\n          <object class=\"GtkGrid\" id=\"grid1\">\n            <property name=\"visible\">True</property>\n            <property name=\"can_focus\">False</property>\n            <child>\n              <object class=\"GtkSpinButton\" id=\"spinbuttonStart\">\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">True</property>\n                <property name=\"adjustment\">adjustmentStart</property>\n              </object>\n              <packing>\n                <property name=\"left_attach\">1</property>\n                <property name=\"top_attach\">0</property>\n                <property name=\"width\">1</property>\n                <property name=\"height\">1</property>\n              </packing>\n            </child>\n            <child>\n              <object class=\"GtkSpinButton\" id=\"spinbuttonStop\">\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">True</property>\n                <property name=\"adjustment\">adjustmentStop</property>\n              </object>\n              <packing>\n                <property name=\"left_attach\">1</property>\n                <property name=\"top_attach\">1</property>\n                <property name=\"width\">1</property>\n                <property name=\"height\">1</property>\n              </packing>\n            </child>\n            <child>\n              <object class=\"GtkSpinButton\" id=\"spinbuttonStep\">\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">True</property>\n                <property name=\"adjustment\">adjustmentStep</property>\n              </object>\n              <packing>\n                <property name=\"left_attach\">1</property>\n                <property name=\"top_attach\">2</property>\n                <property name=\"width\">1</property>\n                <property name=\"height\">1</property>\n              </packing>\n            </child>\n            <child>\n              <object class=\"GtkLabel\" id=\"label11\">\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">False</property>\n                <property name=\"xpad\">10</property>\n                <property name=\"label\" translatable=\"yes\">Start:</property>\n              </object>\n              <packing>\n                <property name=\"left_attach\">0</property>\n                <property name=\"top_attach\">0</property>\n                <property name=\"width\">1</property>\n                <property name=\"height\">1</property>\n              </packing>\n            </child>\n            <child>\n              <object class=\"GtkLabel\" id=\"label12\">\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">False</property>\n                <property name=\"xpad\">10</property>\n                <property name=\"label\" translatable=\"yes\">End:</property>\n              </object>\n              <packing>\n                <property name=\"left_attach\">0</property>\n                <property name=\"top_attach\">1</property>\n                <property name=\"width\">1</property>\n                <property name=\"height\">1</property>\n              </packing>\n            </child>\n            <child>\n              <object class=\"GtkLabel\" id=\"label13\">\n                <property name=\"visible\">True</property>\n                <property name=\"can_focus\">False</property>\n                <property name=\"xpad\">10</property>\n                <property name=\"label\" translatable=\"yes\">Step:</property>\n                <property name=\"ellipsize\">end</property>\n              </object>\n              <packing>\n                <property name=\"left_attach\">0</property>\n                <property name=\"top_attach\">2</property>\n                <property name=\"width\">1</property>\n                <property name=\"height\">1</property>\n              </packing>\n            </child>\n          </object>\n          <packing>\n            <property name=\"expand\">False</property>\n            <property name=\"fill\">False</property>\n            <property name=\"position\">1</property>\n          </packing>\n        </child>\n      </object>\n    </child>\n  </object>\n  <object class=\"GtkMenu\" id=\"menuAudioSamples\">\n    <property name=\"visible\">True</property>\n    <property name=\"can_focus\">False</property>\n    <child>\n      <object class=\"GtkMenuItem\" id=\"menuitemAdd\">\n        <property name=\"visible\">True</property>\n        <property name=\"can_focus\">False</property>\n        <property name=\"label\" translatable=\"yes\">Add Samples</property>\n        <property name=\"use_underline\">True</property>\n      </object>\n    </child>\n    <child>\n      <object class=\"GtkMenuItem\" id=\"menuitemRemove\">\n        <property name=\"visible\">True</property>\n        <property name=\"can_focus\">False</property>\n        <property name=\"label\" translatable=\"yes\">Remove Sample</property>\n        <property name=\"use_underline\">True</property>\n      </object>\n    </child>\n    <child>\n      <object class=\"GtkMenuItem\" id=\"menuitemSelectFC\">\n        <property name=\"visible\">True</property>\n        <property name=\"can_focus\">False</property>\n        <property name=\"label\" translatable=\"yes\">Select all with this FileChannel...</property>\n        <property name=\"use_underline\">True</property>\n      </object>\n    </child>\n    <child>\n      <object class=\"GtkMenuItem\" id=\"menuitemToNewHS\">\n        <property name=\"visible\">True</property>\n        <property name=\"can_focus\">False</property>\n        <property name=\"label\" translatable=\"yes\">Add To New Hit Sample...</property>\n        <property name=\"use_underline\">True</property>\n      </object>\n    </child>\n    <child>\n      <object class=\"GtkMenuItem\" id=\"menuitemAddToMultipleHits\">\n        <property name=\"visible\">True</property>\n        <property name=\"can_focus\">False</property>\n        <property name=\"label\" translatable=\"yes\">Add To Multiple New Hits...</property>\n        <property name=\"use_underline\">True</property>\n      </object>\n    </child>\n  </object>\n  <object class=\"GtkMenu\" id=\"menuHits\">\n    <property name=\"visible\">True</property>\n    <property name=\"can_focus\">False</property>\n    <child>\n      <object class=\"GtkMenuItem\" id=\"menuitemAddHitSample\">\n        <property name=\"visible\">True</property>\n        <property name=\"can_focus\">False</property>\n        <property name=\"tooltip_text\" translatable=\"yes\">Adds a new Hit in this Instrument</property>\n        <property name=\"label\" translatable=\"yes\">Add Hit</property>\n        <property name=\"use_underline\">True</property>\n      </object>\n    </child>\n    <child>\n      <object class=\"GtkMenuItem\" id=\"menuitemAddMultiple\">\n        <property name=\"visible\">True</property>\n        <property name=\"can_focus\">False</property>\n        <property name=\"label\" translatable=\"yes\">Add Multiple Hits...</property>\n        <property name=\"use_underline\">True</property>\n      </object>\n    </child>\n    <child>\n      <object class=\"GtkMenuItem\" id=\"menuitemRemoveHitSample\">\n        <property name=\"visible\">True</property>\n        <property name=\"can_focus\">False</property>\n        <property name=\"tooltip_text\" translatable=\"yes\">Removed the selected Hit from the Instrument</property>\n        <property name=\"label\" translatable=\"yes\">Remove Hit</property>\n        <property name=\"use_underline\">True</property>\n      </object>\n    </child>\n    <child>\n      <object class=\"GtkMenuItem\" id=\"menuitemEqualHP\">\n        <property name=\"visible\">True</property>\n        <property name=\"can_focus\">False</property>\n        <property name=\"label\" translatable=\"yes\">Set Equal Hit Power</property>\n        <property name=\"use_underline\">True</property>\n      </object>\n    </child>\n    <child>\n      <object class=\"GtkMenuItem\" id=\"menuitemSetHitPower\">\n        <property name=\"visible\">True</property>\n        <property name=\"can_focus\">False</property>\n        <property name=\"label\" translatable=\"yes\">Set Linear Hit Power...</property>\n        <property name=\"use_underline\">True</property>\n      </object>\n    </child>\n  </object>\n</interface>"

