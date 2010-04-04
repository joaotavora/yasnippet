;; .yas-setup.el for ruby-mode
;; -*- coding: utf-8 -*-
;;
;; um caralho lhecolheco lhao lhao

;; Substitutions for: content
;;
;; My work in progress substitutions
;;
;; ${1/.+/(/}                                                                        =yyas> ${1:$(and (yas/text) "(")}
;; ${1/.+/)/}                                                                        =yyas> ${1:$(and (yas/text) ")")}
;; ${2/.+/ => /}                                                                     =yyas> ${2:$(and (yas/text) " => ")}
;; ${1:${TM_FILENAME/\.\w+//}                                                        =yyas> ${1:$(and buffer-file-name (file-name-sans-extension buffer-file-name))}
;; ${1/(^.*?\S.*)|.*/(?1:\()/}                                                       =yyas> ${1:$(and (string-match "[^\s\t]" yas/text) "(" )}
;; ${1/(^.*?\S.*)|.*/(?1:\))/}                                                       =yyas> ${1:$(and (string-match "[^\s\t]" yas/text) ")" )}
;; ${2/(^.*?\S.*)|.*/(?1:\()/}                                                       =yyas> ${2:$(and (string-match "[^\s\t]" yas/text) "(" )}
;; ${2/(^.*?\S.*)|.*/(?1:\))/}                                                       =yyas> ${2:$(and (string-match "[^\s\t]" yas/text) ")" )}
;; ${3/(^.*?\S.*)|.*/(?1:\()/}                                                       =yyas> ${3:$(and (string-match "[^\s\t]" yas/text) "(" )}
;; ${3/(^.*?\S.*)|.*/(?1:\))/}                                                       =yyas> ${3:$(and (string-match "[^\s\t]" yas/text) ")" )}
;; ${2/^\s*$|(.*\S.*)/(?1: )/}                                                       =yyas> ${2:$(and (string-match "[^\s\t]" yas/text) " " )}
;; ${3/^\s*$|(.*\S.*)/(?1: )/}                                                       =yyas> ${3:$(and (string-match "[^\s\t]" yas/text) " " )}
;;
;; Substitutions for: condition
;;
;; (string.quoted.double.ruby|string.interpolated.ruby) - string source                       =yyas> (yas/ruby-in-interpolated-string-p)
;; text.html.ruby, text.html source.ruby                                                      =yyas> (yas/unimplemented)
;; text.html, source.yaml, meta.erb                                                           =yyas> (yas/unimplemented)
;; keyword.control.start-block.ruby, meta.syntax.ruby.start-block                             =yyas>
;; 
;; Substitutions for: binding
;;
;; # as in Commands/New Method.yasnippet
;; $                                                                                        =yyas> C-s-M
;; --**--
;; Automatically generated code, do not edit this part
;; 
;; Translated menu
;; 
(yas/define-menu 'ruby-mode
                 '((yas/external-item "35222962-C50D-4D58-A6AE-71E7AD980BE4")
                   (yas/item "5289EE40-86B8-11D9-A8D4-000A95E13C98")
                   (yas/separator)
                   (yas/item "63F3B3B7-CBE2-426B-B551-657733F3868B")
                   (yas/submenu "RDoc"
                                ((yas/item "1AD6A138-2E89-4D6A-AB3F-416BF9CE968D")
                                 (yas/separator)
                                 (yas/submenu "Format"
                                              ((yas/external-item "931DD73E-615E-476E-9B0D-8341023AE730")
                                               (yas/external-item "DAA69A0C-FC1E-4509-9931-DFFB38B4D6AE")
                                               (yas/external-item "2DDB6FE0-6111-4C40-A149-8E67E76F8272")))
                                 (yas/separator)
                                 (yas/item "05984208-D559-4C04-A69C-2019361A985A")
                                 (yas/external-item "BF4CA9F1-51CD-48D4-8357-852234F59046")
                                 (yas/separator)
                                 (yas/item "ED6368FB-A11D-4622-9F42-7879481094F1")))
                   (yas/separator)
                   (yas/external-item "8646378E-91F5-4771-AC7C-43FC49A93576")
                   (yas/item "EE5F19BA-6C02-11D9-92BA-0011242E4184")
                   (yas/separator)
                   (yas/item "EE5F1FB2-6C02-11D9-92BA-0011242E4184")
                   (yas/external-item "FBFC214F-B019-4967-95D2-028F374A3221")
                   (yas/item "88BC3896-DC39-4307-A271-21D33340F15A")
                   (yas/separator)
                   (yas/item "9FB64639-F776-499B-BA6F-BB45F86F80FD")
                   (yas/item "7F79BC8D-8A4F-4570-973B-05DFEC25747F")
                   (yas/external-item "B297E4B8-A8FF-49CE-B9C4-6D4911724D43")
                   (yas/item "FDFABCB9-DF58-4469-AE11-5407A4FF4D70")
                   (yas/separator)
                   (yas/submenu "Declarations"
                                ((yas/item "0275EF39-9357-408F-AF20-79E415CA9504")
                                 (yas/separator)
                                 (yas/item "0F940CBC-2173-49FF-B6FD-98A62863F8F2")
                                 (yas/item "667083EE-62C3-11D9-B8CF-000D93589AF6")
                                 (yas/item "48D8E498-C9A5-4B1B-9A18-71A5860276FB")
                                 (yas/item "4E9A7A73-875C-11D9-897C-000393CBCE2E")
                                 (yas/item "6670835F-62C3-11D9-B8CF-000D93589AF6")
                                 (yas/item "667082E6-62C3-11D9-B8CF-000D93589AF6")
                                 (yas/item "CD1609FA-47DA-4EE4-9C5B-5C56D953F5B1")
                                 (yas/item "F53E098D-D08E-4CE2-990A-B0BD70E60614")
                                 (yas/item "D121FC61-96A4-4B8F-8709-280EDA876FF3")
                                 (yas/item "488B387C-50C0-4B2D-9260-5A7E7EAF9B42")
                                 (yas/separator)
                                 (yas/submenu "Classes and Modules"
                                              ((yas/item "BF487539-8085-4FF4-8601-1AD20FABAEDC")
                                               (yas/item "83EED068-8C1C-4BAF-9893-902DC00616AB")
                                               (yas/item "0CCBE04E-F4E2-4E55-9506-7DE67ACF8388")
                                               (yas/item "05DFF82C-5A29-4EBD-93FE-C165FFFB5EA8")
                                               (yas/item "E98FB8F9-7302-431D-8BF2-275A68A6126C")
                                               (yas/item "121B334B-2AA6-4E9A-A8B8-BF93B627982B")
                                               (yas/item "AFE1D078-EA16-45F5-AD8A-FAC1B523D861")
                                               (yas/item "C7AAAE45-487A-4B61-8962-D47675AAC05F")
                                               (yas/separator)
                                               (yas/item "2B73EC5F-06D2-460C-A14F-6FA05AFCF0CC")
                                               (yas/item "0E85EC81-2FAB-4648-B590-119CC1BB6E41")
                                               (yas/item "A71A18CF-2D71-4BFF-AA0C-D9B8C59BC4EB")))
                                 (yas/submenu "Methods"
                                              ((yas/item "A150C2D8-25B3-4339-BC92-8A0160A70486")
                                               (yas/item "3D383096-A03F-4EF8-9060-3C727045AB34")
                                               (yas/item "D7A7D3C9-1714-4C50-8CC0-D83A03883E8F")
                                               (yas/separator)
                                               (yas/item "AAD5D511-6BE7-41DA-8F2B-1593A48FBB08")
                                               (yas/item "6C9D6B3D-D8E9-4606-9534-577C8D21FFF6")
                                               (yas/separator)
                                               (yas/item "58FDEA60-10AF-4C49-AA09-29B77030DB25")
                                               (yas/item "7F46C90A-595B-4B83-A4F7-058F63CE4218")
                                               (yas/separator)
                                               (yas/item "7C6E88FA-CA0E-4110-8C75-A94E54286A75")
                                               (yas/item "87D5F8AD-8DA6-4AED-A0D8-B51CAC980445")
                                               (yas/item "C44ED391-614F-4BA2-BB0F-87668EEA9954")
                                               (yas/item "4A6EFD6B-88E2-4822-AD48-03460EDBC796")
                                               (yas/separator)
                                               (yas/item "988C8AEF-FC71-4455-9C4F-9338C05685A4")))
                                 (yas/item "451A0596-1F72-4AFB-AF2F-45900FABB0F7")
                                 (yas/separator)
                                 (yas/item "A05CBDD6-845D-45EB-94FB-F8787F5456BE")
                                 (yas/item "97DE939B-D243-4D5C-B953-1C9090912E7C")
                                 (yas/item "33969819-62C5-4E03-B824-C2337205F364")
                                 (yas/item "34FEBB9F-73CD-4DD4-A0A3-1CF2A5E3DE78")
                                 (yas/item "E16D24D2-CC7E-4786-BE0B-1725FC865D78")
                                 (yas/item "21C0D711-F32A-4665-AA0D-B136F9DD3945")
                                 (yas/item "49D69DEC-6991-49F4-8D9B-BA60BFDD3D17")))
                   (yas/submenu "Iterators"
                                ((yas/submenu "Arrays"
                                              ((yas/item "DAE6A754-D906-4763-B816-CE67125CEF08")
                                               (yas/separator)
                                               (yas/item "263C94DC-63CF-4BA3-9692-C5582CA8F1AB")
                                               (yas/item "6021BBDC-4AAD-447B-A0C2-A4BB31721558")
                                               (yas/item "3DDB99C4-486D-4C11-A217-5680FDD8EC19")
                                               (yas/item "FD010022-E0E7-44DB-827F-33F7D9310DA2")))
                                 (yas/submenu "Counting"
                                              ((yas/item "4991BB86-736E-4758-B9B2-E4FA90B9368F")
                                               (yas/item "36853A11-0307-4AE7-B835-7CE6358717A5")
                                               (yas/item "206D54AF-E67A-4DF0-B7F4-3D42FEB81685")
                                               (yas/item "51954118-81D7-42B6-9A10-BE23D8B9FFE2")
                                               (yas/separator)
                                               (yas/item "567E3D18-BF2B-4379-8927-2777EC9F495E")))
                                 (yas/submenu "Each Element"
                                              ((yas/item "ECBA4CA0-275F-460E-85BE-E82FEA2E2B26")
                                               (yas/item "338EC03D-3FF4-4435-94E8-1CEF20CEC75D")
                                               (yas/item "7E084412-80E6-4B70-8092-C03D1ECE4CD2")
                                               (yas/item "FDD73070-6D32-4301-A86A-C55B77C3D8ED")
                                               (yas/item "EC73D5CC-5F05-46B9-A6F4-82037E4A38C9")
                                               (yas/item "3C04589C-5127-478E-97B3-CA7DD2EA7ECD")
                                               (yas/item "689120C9-AB40-4081-8268-9362E00FA4A0")
                                               (yas/item "E54F7077-3C33-4B53-A4F7-21E16132D3AD")
                                               (yas/item "02913388-EE8E-4C55-AC94-94F3D751F47E")
                                               (yas/item "7A3CECED-452B-438E-A5C6-95B6BDC43243")
                                               (yas/item "825B721D-4367-4DF7-98C0-F005695DF9E3")
                                               (yas/item "CD748479-D2A4-4AB5-95BD-4C89512BA210")
                                               (yas/item "844DBD70-BC23-4FBF-9C18-F4A610239DF2")
                                               (yas/item "1DD13CF5-39C0-4F10-B655-56DACEBC7F94")
                                               (yas/item "F3C5F719-EF03-4FF7-A777-4A8402FE3B6B")
                                               (yas/separator)
                                               (yas/item "B563E0D7-513D-49B4-9733-1B04A6F25A74")
                                               (yas/separator)
                                               (yas/item "5A3754FC-43A3-462B-AB42-E3E951872E6F")
                                               (yas/item "BFB65D1C-62F1-485D-8A67-3E5A2E55107C")
                                               (yas/item "BD4CFD7B-1AC0-4569-9BDA-FD491F41F4E6")))
                                 (yas/submenu "Ordering"
                                              ((yas/item "9E0B4D4B-2956-4B3A-800A-3D8CE54E66BF")
                                               (yas/item "BA9440C9-36C3-4031-BB61-67B581D5B179")
                                               (yas/separator)
                                               (yas/item "B0CE57EC-FB2E-4482-8CCE-448DC2588715")))
                                 (yas/submenu "Searching and Selection"
                                              ((yas/item "07D1F987-7CDB-4EAD-B64A-27A93051700E")
                                               (yas/item "A3B9B76B-2BC5-425C-AB24-9FAAFC375798")
                                               (yas/item "5DA9E1E8-2C54-420A-9B84-B040A1AF2B9E")
                                               (yas/item "669A86AD-936F-4EDA-8E4E-6863804072DA")
                                               (yas/item "6C6B9849-9631-49FF-A9F9-F0E94A1512C5")
                                               (yas/item "1F72122A-35AD-4BA1-AA01-889A10319666")
                                               (yas/item "E23FE534-8061-4828-98A5-46270B6910B0")
                                               (yas/item "197709C5-8382-4A59-B6D7-31A0CC0F23B7")
                                               (yas/item "9D9E7BA3-8C5D-4532-83EA-326358C2F5BB")
                                               (yas/item "98182B9E-7C61-4824-BE4C-9CD69C816037")
                                               (yas/item "CB03D11A-7204-48D0-92C1-E109034403E7")
                                               (yas/item "52B8BF63-F09E-4789-8407-06168A8AE666")
                                               (yas/item "B79B9DAB-ABEF-44F6-BF7E-635E7BA11DFD")
                                               (yas/item "4E409AA4-E7D4-46B7-A4E9-E32F992B33E9")))
                                 (yas/submenu "Strings"
                                              ((yas/item "8021944C-CEA4-4983-8D1C-78D18D4004A1")
                                               (yas/item "2514FC26-468C-4D08-A788-494A444C4286")
                                               (yas/separator)
                                               (yas/item "66802933-B49F-479B-9DF9-1D898FF1FA90")))))
                   (yas/submenu "Blocks"
                                ((yas/item "7990EE60-C850-4779-A8C0-7FD2C853B99B")
                                 (yas/separator)
                                 (yas/item "855FC4EF-7B1E-48EE-AD4E-5ECB8ED79D1C")
                                 (yas/item "4B72C5C3-6CA7-41AC-B2F9-51DEA25D469E")
                                 (yas/separator)
                                 (yas/item "21E75321-0CF7-45E8-A297-BCC7C0DDDD15")))
                   (yas/submenu "Hashes"
                                ((yas/item "E16EE658-1CA0-4950-954B-B962E50B754F")
                                 (yas/separator)
                                 (yas/item "840B9C4C-7037-4C3B-9028-EB9DC75EDB3E")
                                 (yas/item "B9E3A6DF-875D-11D9-897C-000393CBCE2E")))
                   (yas/submenu "Files"
                                ((yas/item "418F1817-255F-430A-B09A-222964ED66A7")
                                 (yas/separator)
                                 (yas/item "8F594E5E-6F46-4E98-B5FB-1C8F3BA9828F")
                                 (yas/item "50C56AC8-48F3-42A0-AF10-8164464AFAEF")
                                 (yas/item "397FA09F-A30F-4EE4-920C-318D5004EE97")
                                 (yas/separator)
                                 (yas/item "332AA973-AA71-48CB-AEE9-1D71E11019AC")
                                 (yas/item "8EBBB26F-980E-404E-8366-74E5772298F6")
                                 (yas/item "678BDB83-FBBD-4E8E-BE0B-E1A98AECB247")
                                 (yas/item "A4E89D97-D5ED-48BB-B5FF-1BFB79211FCD")
                                 (yas/separator)
                                 (yas/item "8CEF9711-88D5-4202-AFB9-29EF4EFD25C1")
                                 (yas/item "C3C48948-4F49-484E-A8DE-DEB44723099E")
                                 (yas/item "209D5D73-7A77-4931-A158-3FB6D5B48A88")))
                   (yas/submenu "Tests"
                                ((yas/item "31D1F145-33AB-4441-BA11-4D1C46928C4C")
                                 (yas/item "00F66D41-25AF-4597-B67D-E540965A5222")
                                 (yas/item "5297FD0C-98B1-4514-BBD1-1516810BECA6")
                                 (yas/separator)
                                 (yas/item "B32C147D-44A6-478A-9D5D-189D7831E9A7")
                                 (yas/item "43A61A22-6BEE-4997-961C-1CDE739C05FE")
                                 (yas/item "A243E96F-DC21-4AA0-B340-13A7674F6AFF")
                                 (yas/item "429D0EF5-580D-4166-8F79-713DE96B77F1")
                                 (yas/item "0E831E03-67E1-4357-8323-C60685C23C4F")
                                 (yas/item "671F05E2-D9CC-485E-BB1B-B13EF20FAC65")
                                 (yas/item "4C79256C-480A-459C-BDE8-BB0D972811DB")
                                 (yas/item "79FEC3CC-2A40-4611-9A85-ECDB22FE0701")
                                 (yas/item "711ED6C3-0F18-41FB-9A7D-3094BB319A85")
                                 (yas/item "A072BB1E-1DD1-45D3-9346-8CA3BA21B364")
                                 (yas/item "1B925A4D-8EE4-442B-9254-293599F5717F")
                                 (yas/item "68B21F6F-5D89-41FA-A19C-F29C2F912B4E")
                                 (yas/item "82F8EEE0-2452-411E-8102-7BFDDBCA2E72")
                                 (yas/item "09A11FDA-49FC-4466-8787-8D1D5D111A89")
                                 (yas/item "29340695-E426-4F77-8CF7-C59360A549F4")
                                 (yas/item "F91C25EC-EC76-498B-BFB5-FDA8F57C5875")
                                 (yas/item "7850AD5C-A90D-4E2C-A931-EADFF8D3D9A3")
                                 (yas/item "05655BD8-23C6-445F-BFD1-420BF25C3030")
                                 (yas/item "33639D7A-BD8C-4396-9C44-307B8AC87C9E")
                                 (yas/item "DB457094-1AC9-4856-AEFC-43A9576B6775")
                                 (yas/separator)
                                 (yas/item "C649F945-DAB8-4DA2-B73C-2EFF9D7D34F3")
                                 (yas/item "942F20E2-C40A-44B8-A3F2-99AAC68CB534")
                                 (yas/item "1C60D589-DD46-4109-90CA-6B34AEA2F298")))
                   (yas/submenu "Serialization"
                                ((yas/item "0CB48BCA-3F6E-4AE0-85BC-08A1D2508216")
                                 (yas/item "20AAD0BC-075D-4EC0-9057-E3E5E62C4125")
                                 (yas/separator)
                                 (yas/item "5AE7CFB4-418E-4E00-AD76-06DB755EE876")
                                 (yas/item "5B46ECFD-23A4-4F0C-9951-F64C19C72C2B")
                                 (yas/item "46BF99AD-E172-4D49-BCF7-072F4730E1D9")
                                 (yas/separator)
                                 (yas/item "9460392B-C036-4A76-A5AE-1191F10E4B1B")
                                 (yas/item "3BA6762A-BB6B-489E-8006-F30F386AEF48")
                                 (yas/item "2C07D4E7-D74F-4AE4-82BE-B0BA82247AFA")
                                 (yas/item "8343ACF4-EEB7-44B5-B835-94826466D4D5")
                                 (yas/separator)
                                 (yas/item "F6BF907E-FDF7-4D9B-9E57-BE159561349D")
                                 (yas/item "B904D4AA-D15D-48A4-8EB2-563BAF489332")
                                 (yas/item "CC300D44-6C3F-4F6C-A8AB-86F5A2DC57CF")))
                   (yas/submenu "Idioms"
                                ((yas/item "2DBEE50B-3097-4A57-AB48-3586CF392D8B")
                                 (yas/item "0BA2B2F1-E767-4A03-9791-0AC0183251F1")
                                 (yas/item "B46D35B8-5DEB-4C10-A110-BA1965A2EB9C")
                                 (yas/item "97054C4D-E4A3-45B1-9C00-B82DBCB30CAD")))
                   (yas/submenu "Rake"
                                ((yas/external-item "569C9822-8C41-4907-94C7-1A8A0031B66D")
                                 (yas/separator)
                                 (yas/item "05EE1046-5ED7-48F5-8693-1F066163B2F4")
                                 (yas/item "A7BF14E6-59B1-42E5-8755-8A72BF13685E"))))
                    '("Overwrite '}' in #{ .. }"
                       "Embedded Code — #{…}"
                       "Validate Syntax (ERB)"
                       "Toggle Quote Style"
                       "835FAAC6-5431-436C-998B-241F7226B99B"
                       "Delete forward/backward"
                       "47D203ED-EB9B-4653-A07B-A897800CEB76"))

;; Unknown substitutions
;; 
;; Substitutions for: content
;; 
;; # as in Snippets/open(pathorurl, w) do doc .. end (ope).yasnippet
;; ${3/(^[rwab+]+$)|.*/(?1:, ")/}                                                             =yyas> (yas/unknown)
;; 
;; # as in Snippets/open(pathorurl, w) do doc .. end (ope).yasnippet
;; ${3/(^[rwab+]+$)|.*/(?1:")/}                                                               =yyas> (yas/unknown)
;; 
;; # as in Snippets/upto(1.00.0) { n .. } (upt).yasnippet
;; ${2/(^(?<var>\s*(?:\*|\*?[a-z_])[a-zA-Z0-9_]*\s*)(,\g<var>)*,?\s*$)|.*/(?1:|)/}            =yyas> (yas/unknown)
;; 
;; # as in Snippets/upto(1.00.0) { n .. } (upt).yasnippet
;; ${2/(^(?<var>\s*(?:\*|\*?[a-z_])[a-zA-Z0-9_]*\s*)(,\g<var>)*,?\s*$)|.*/(?1:| )/}           =yyas> (yas/unknown)
;; 
;; # as in Snippets/Wrap in Begin Rescue End.yasnippet
;; ${TM_SELECTED_TEXT/([\t ]*).*/$1/m}                                                        =yyas> (yas/unknown)
;; 
;; # as in Snippets/Wrap in Begin Rescue End.yasnippet
;; ${TM_SELECTED_TEXT/(\A.*)|(.+)|\n\z/(?1:$0:(?2:\t$0))/g}                                   =yyas> (yas/unknown)
;; 
;; # as in Snippets/module .. module_function .. end.yasnippet
;; ${TM_FILENAME/(?:\A|_)([A-Za-z0-9]+)(?:\.rb)?/(?2::\u$1)/g}                                =yyas> (yas/unknown)
;; 
;; # as in Snippets/class .. TestUnitTestCase .. end (tc).yasnippet
;; ${1/([\w&&[^_]]+)|./\u$1/g}                                                                =yyas> (yas/unknown)
;; 
;; # as in Snippets/do obj .. end (doo).yasnippet
;; ${1/(^(?<var>\s*[a-z_][a-zA-Z0-9_]*\s*)(,\g<var>)*,?\s*$)|.*/(?1: |)/}                     =yyas> (yas/unknown)
;; 
;; # as in Snippets/open yield block ({).yasnippet
;; ${1/(^(?<var>\s*[a-z_][a-zA-Z0-9_]*\s*)(,\g<var>)*,?\s*$)|.*/(?1:|)/}                      =yyas> (yas/unknown)
;; 
;; # as in Snippets/times { n .. } (tim).yasnippet
;; ${1/(^(?<var>\s*(?:\*|\*?[a-z_])[a-zA-Z0-9_]*\s*)(,\g<var>)*,?\s*$)|.*/(?1:|)/}            =yyas> (yas/unknown)
;; 
;; # as in Snippets/times { n .. } (tim).yasnippet
;; ${1/(^(?<var>\s*(?:\*|\*?[a-z_])[a-zA-Z0-9_]*\s*)(,\g<var>)*,?\s*$)|.*/(?1:| )/}           =yyas> (yas/unknown)
;; 
;; # as in Snippets/open yield block ({).yasnippet
;; ${1/(^(?<var>\s*[a-z_][a-zA-Z0-9_]*\s*)(,\g<var>)*,?\s*$)|.*/(?1:| )/}                     =yyas> (yas/unknown)
;; 
;; # as in Snippets/untitled.yasnippet
;; ${3/^\s*$|(.*\S.*)/(?1:, )/}                                                               =yyas> (yas/unknown)
;; 
;; 

;; Substitutions for: condition
;; 
;; 

;; Substitutions for: binding
;; 
;; # as in Snippets/Insert ERb's __ or = __.yasnippet
;; ^>                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Check Ruby Syntax.yasnippet
;; ^V                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Execute Line with Ruby.yasnippet
;; ^E                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Insert Missing Requires.yasnippet
;; ^#                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Lookup in Documentation.yasnippet
;; ^h                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Make Destructive Call.yasnippet
;; ^!                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Run focused unit test.yasnippet
;; @R                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Toggle Quote Style.yasnippet
;; ^"                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Macros/Overwrite } in #{ .. }.yasnippet
;; }                                                                                          =yyas> (yas/unknown)
;; 
;; # as in Macros/Toggle Single Multi Line Block.yasnippet
;; ^{                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Snippets/Wrap in Begin Rescue End.yasnippet
;; ^W                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Snippets/embed string variable.yasnippet
;; #                                                                                          =yyas> (yas/unknown)
;; 
;; # as in Snippets/hash pointer.yasnippet
;; ^l                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Macros/Delete forwardbackward.yasnippet
;;                                                                                           =yyas> (yas/unknown)
;; 
;; 

;; .yas-setup.el for ruby-mode ends here
