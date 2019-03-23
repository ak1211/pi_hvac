{-
 PiHVAC <https://github.com/ak1211/pi_hvac>
 Copyright 2019 Akihiro Yamamoto

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
-}

module Page.About
  ( Query(..)
  , component
  ) where

import Prelude

import AppM (class Navigate, navigate)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as HB
import Page.Commons as Commons
import Route (Route)
import Route as Route

type State =
  { session :: String
  }

data Query a
  = Initialize a
  | NavigateTo Route a

-- | child component
component
  :: forall m
   . MonadAff m
  => Navigate m
  => H.Component HH.HTML Query Unit Void m
component =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , initializer: Just (H.action Initialize)
    , finalizer: Nothing
    , receiver: const Nothing
    }
  where

  initialState =
    { session: "About"
    }

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    Initialize next -> do
      pure next

    NavigateTo route next -> do
      navigate route
      pure next


render :: State -> H.ComponentHTML Query
render state =
  HH.div_
    [ Commons.navbar NavigateTo Route.About
    , HH.div [ HP.class_ $ HC.ClassName "container" ] document
    ]

document :: forall p i. Array (H.HTML p i)
document =
  [ HH.h2 [ HP.class_ HB.h2 ] [ HH.text "About this application" ]
  , HH.p
    [ HP.classes [ HB.m4 ]
    ]
    [ HH.text "PiHVAC <"
    , HH.a [ HP.href repo ] [ HH.text repo ]
    , HH.text ">"
    , HH.br_
    , HH.text copyright
    ]
  , HH.h2 [ HP.class_ HB.h2 ] [ HH.text "The License" ]
  , HH.pre
    [ HP.class_ HB.m4 ]
    [ HH.text apacheLicenseV2 ]
  , HH.h2 [ HP.class_ HB.h2 ] [ HH.text "External libraries" ]
  , HH.p
    [ HP.class_ HB.m4 ]
    [ HH.text "This software may contain all or part of the program specified in notice files below."
    , HH.br_
    , HH.strong_ [ HH.text "No NOTICE files specified." ]
    ]
  , HH.hr_
  , HH.p
    [ HP.class_ HB.m4 ]
    [ HH.text "The program includes software licensed under the license terms as specified below." ]
  , HH.ul_
    [ HH.li_
      [ HH.text "purescript-affjax"
      , HH.p_
        [ HH.text "Licensed under the Apache License, Version 2.0"
        , HH.br_
        , HH.a
          [ HP.href "https://github.com/slamdata/purescript-affjax" ]
          [ HH.text "see LICENSE file for Details." ]
        ]
      ]
    , HH.li_
      [ HH.text "purescript-assert"
      , HH.p_
        [ HH.text "Copyright 2018 PureScript"
        , HH.br_
        , HH.text "Licensed under the BSD 3-Clause License"
        , HH.br_
        , HH.a
          [ HP.href "https://github.com/purescript/purescript-assert" ]
          [ HH.text "see LICENSE file for Details." ]
        ]
      ]
    , HH.li_
      [ HH.text "purescript-canvas"
      , HH.p_
        [ HH.text "Copyright (c) 2014-18 Phil Freeman and other contributors"
        , HH.br_
        , HH.text "Licensed under the MIT license."
        , HH.br_
        , HH.a
          [ HP.href "https://github.com/paf31/purescript-canvas" ]
          [ HH.text "see LICENSE file for Details." ]
        ]
      ]
    , HH.li_
      [ HH.text "purescript-console"
      , HH.p_
        [ HH.text "Copyright 2018 PureScript"
        , HH.br_
        , HH.text "Licensed under the BSD 3-Clause License"
        , HH.br_
        , HH.a
          [ HP.href "https://github.com/purescript/purescript-console" ]
          [ HH.text "see LICENSE file for Details." ]
        ]
      ]
    , HH.li_
      [ HH.text "purescript-datetime"
      , HH.p_
        [ HH.text "Copyright 2018 PureScript"
        , HH.br_
        , HH.text "Licensed under the BSD 3-Clause License"
        , HH.br_
        , HH.a
          [ HP.href "https://github.com/purescript/purescript-datetime" ]
          [ HH.text "see LICENSE file for Details." ]
        ]
      ]
    , HH.li_
      [ HH.text "purescript-effect"
      , HH.p_
        [ HH.text "Copyright 2018 PureScript"
        , HH.br_
        , HH.text "Licensed under the BSD 3-Clause License"
        , HH.br_
        , HH.a
          [ HP.href "https://github.com/purescipt/purescript-effect" ]
          [ HH.text "see LICENSE file for Details." ]
        ]
      ]
    , HH.li_
      [ HH.text "purescript-foreign-generic"
      , HH.p_
        [ HH.text "Copyright (c) 2017 Phil Freeman"
        , HH.br_
        , HH.text "Licensed under the MIT license."
        , HH.br_
        , HH.a
          [ HP.href "https://github.com/paf31/purescript-foreign-generic" ]
          [ HH.text "see LICENSE file for Details." ]
        ]
      ]
    , HH.li_
      [ HH.text "purescript-formatters"
      , HH.p_
        [ HH.text "Licensed under the Apache License, Version 2.0"
        , HH.br_
        , HH.a
          [ HP.href "https://github.com/slamdata/purescript-formatters" ]
          [ HH.text "see LICENSE file for Details." ]
        ]
      ]
    , HH.li_
      [ HH.text "purescript-halogen"
      , HH.p_
        [ HH.text "Licensed under the Apache License, Version 2.0"
        , HH.br_
        , HH.a
          [ HP.href "https://github.com/slamdata/purescript-halogen" ]
          [ HH.text "see LICENSE file for Details." ]
        ]
      ]
    , HH.li_
      [ HH.text "purescript-halogen-bootstrap4"
      , HH.p_
        [ HH.text "Licensed under the Apache License, Version 2.0"
        , HH.br_
        , HH.a
          [ HP.href "https://github.com/mschristiansen/purescript-halogen-bootstrap4" ]
          [ HH.text "see LICENSE file for Details." ]
        ]
      ]
    , HH.li_
      [ HH.text "purescript-halogen-css"
      , HH.p_
        [ HH.text "Licensed under the Apache License, Version 2.0"
        , HH.br_
        , HH.a
          [ HP.href "https://github.com/slamdata/purescript-halogen-css" ]
          [ HH.text "see LICENSE file for Details." ]
        ]
      ]
    , HH.li_
      [ HH.text "purescript-now"
      , HH.p_
        [ HH.text "Copyright (c) 2014 Purescript"
        , HH.br_
        , HH.text "Licensed under the MIT license."
        , HH.br_
        , HH.a
          [ HP.href "https://github.com/purescript-contrib/purescript-now" ]
          [ HH.text "see LICENSE file for Details." ]
        ]
      ]
    , HH.li_
      [ HH.text "purescript-numbers"
      , HH.p_
        [ HH.text "Licensed under the MIT license."
        , HH.br_
        , HH.a
          [ HP.href "https://github.com/sharkdp/purescript-numbers" ]
          [ HH.text "see link for Details." ]
        ]
      ]
    , HH.li_
      [ HH.text "purescript-parsing"
      , HH.p_
        [ HH.pre_ [ HH.text """Copyright 2014-2016 PureScript

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice,
  this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer in the
  documentation and/or other materials provided with the distribution.

This software is provided by the copyright holders "as is" and any express or
implied warranties, including, but not limited to, the implied warranties of
merchantability and fitness for a particular purpose are disclaimed. In no
event shall the copyright holders be liable for any direct, indirect,
incidental, special, exemplary, or consequential damages (including, but not
limited to, procurement of substitute goods or services; loss of use, data,
or profits; or business interruption) however caused and on any theory of
liability, whether in contract, strict liability, or tort (including
negligence or otherwise) arising in any way out of the use of this software,
even if advised of the possibility of such damage.

-------------------------------------------------------------------------------

This library uses code adapted from the Haskell libraries parsec and indents.
Their licenses are reproduced below:

'parsec' license:

  Copyright 1999-2000, Daan Leijen; 2007, Paolo Martini. All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  * Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.
  * Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.

  This software is provided by the copyright holders "as is" and any express or
  implied warranties, including, but not limited to, the implied warranties of
  merchantability and fitness for a particular purpose are disclaimed. In no
  event shall the copyright holders be liable for any direct, indirect,
  incidental, special, exemplary, or consequential damages (including, but not
  limited to, procurement of substitute goods or services; loss of use, data,
  or profits; or business interruption) however caused and on any theory of
  liability, whether in contract, strict liability, or tort (including
  negligence or otherwise) arising in any way out of the use of this software,
  even if advised of the possibility of such damage.

'indents' license:

  Copyright (c)2010, Sam Anklesaria

  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sam Anklesaria nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.""" ]
        , HH.a
          [ HP.href "https://github.com/purescript-contrib/purescript-parsing" ]
          [ HH.text "see LICENSE file for Details." ]
        ]
      ]
    , HH.li_
      [ HH.text "purescript-prelude"
      , HH.p_
        [ HH.text "Copyright 2018 PureScript"
        , HH.br_
        , HH.text "Licensed under the BSD 3-Clause License"
        , HH.br_
        , HH.a
          [ HP.href "https://github.com/purescript/purescript-prelude" ]
          [ HH.text "see LICENSE file for Details." ]
        ]
      ]
    , HH.li_
      [ HH.text "purescript-routing"
      , HH.p_
        [ HH.text "Licensed under the Apache License, Version 2.0"
        , HH.br_
        , HH.a
          [ HP.href "https://github.com/slamdata/purescript-routing" ]
          [ HH.text "see LICENSE file for Details." ]
        ]
      ]
    , HH.li_
      [ HH.text "purescript-web-file"
      , HH.p_
        [ HH.text "Copyright 2018 PureScript"
        , HH.br_
        , HH.text "Licensed under the MIT License"
        , HH.br_
        , HH.a
          [ HP.href "https://github.com/purescript-web/purescript-web-file" ]
          [ HH.text "see LICENSE file for Details." ]
        ]
      ]
    , HH.li_
      [ HH.text "HTML Canvas Gauges v2.1"
      , HH.p_
        [ HH.text "Copyright (c) 2016 Mykhailo Stadnyk <mikhus@gmail.com>."
        , HH.br_
        , HH.text "Licensed under the MIT license."
        , HH.br_
        , HH.a
          [ HP.href "https://github.com/Mikhus/canvas-gauges" ]
          [ HH.text "see LICENSE file for Details." ]
        ]
      ]
    , HH.li_
      [ HH.text "Chart.js"
      , HH.p_
        [ HH.text "Copyright (c) 2018 Chart.js Contributors."
        , HH.br_
        , HH.text "Licensed under the MIT license."
        , HH.br_
        , HH.a
          [ HP.href "https://github.com/chartjs/Chart.js" ]
          [ HH.text "see LICENSE file for Details." ]
        ]
      ]
    ]
  ]
  where

  repo =
    "https://github.com/ak1211/pi_hvac"

  copyright =
    """Copyright 2019 Akihiro Yamamoto"""
  
  apacheLicenseV2 = """
                                 Apache License
                           Version 2.0, January 2004
                        http://www.apache.org/licenses/

   TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION

   1. Definitions.

      "License" shall mean the terms and conditions for use, reproduction,
      and distribution as defined by Sections 1 through 9 of this document.

      "Licensor" shall mean the copyright owner or entity authorized by
      the copyright owner that is granting the License.

      "Legal Entity" shall mean the union of the acting entity and all
      other entities that control, are controlled by, or are under common
      control with that entity. For the purposes of this definition,
      "control" means (i) the power, direct or indirect, to cause the
      direction or management of such entity, whether by contract or
      otherwise, or (ii) ownership of fifty percent (50%) or more of the
      outstanding shares, or (iii) beneficial ownership of such entity.

      "You" (or "Your") shall mean an individual or Legal Entity
      exercising permissions granted by this License.

      "Source" form shall mean the preferred form for making modifications,
      including but not limited to software source code, documentation
      source, and configuration files.

      "Object" form shall mean any form resulting from mechanical
      transformation or translation of a Source form, including but
      not limited to compiled object code, generated documentation,
      and conversions to other media types.

      "Work" shall mean the work of authorship, whether in Source or
      Object form, made available under the License, as indicated by a
      copyright notice that is included in or attached to the work
      (an example is provided in the Appendix below).

      "Derivative Works" shall mean any work, whether in Source or Object
      form, that is based on (or derived from) the Work and for which the
      editorial revisions, annotations, elaborations, or other modifications
      represent, as a whole, an original work of authorship. For the purposes
      of this License, Derivative Works shall not include works that remain
      separable from, or merely link (or bind by name) to the interfaces of,
      the Work and Derivative Works thereof.

      "Contribution" shall mean any work of authorship, including
      the original version of the Work and any modifications or additions
      to that Work or Derivative Works thereof, that is intentionally
      submitted to Licensor for inclusion in the Work by the copyright owner
      or by an individual or Legal Entity authorized to submit on behalf of
      the copyright owner. For the purposes of this definition, "submitted"
      means any form of electronic, verbal, or written communication sent
      to the Licensor or its representatives, including but not limited to
      communication on electronic mailing lists, source code control systems,
      and issue tracking systems that are managed by, or on behalf of, the
      Licensor for the purpose of discussing and improving the Work, but
      excluding communication that is conspicuously marked or otherwise
      designated in writing by the copyright owner as "Not a Contribution."

      "Contributor" shall mean Licensor and any individual or Legal Entity
      on behalf of whom a Contribution has been received by Licensor and
      subsequently incorporated within the Work.

   2. Grant of Copyright License. Subject to the terms and conditions of
      this License, each Contributor hereby grants to You a perpetual,
      worldwide, non-exclusive, no-charge, royalty-free, irrevocable
      copyright license to reproduce, prepare Derivative Works of,
      publicly display, publicly perform, sublicense, and distribute the
      Work and such Derivative Works in Source or Object form.

   3. Grant of Patent License. Subject to the terms and conditions of
      this License, each Contributor hereby grants to You a perpetual,
      worldwide, non-exclusive, no-charge, royalty-free, irrevocable
      (except as stated in this section) patent license to make, have made,
      use, offer to sell, sell, import, and otherwise transfer the Work,
      where such license applies only to those patent claims licensable
      by such Contributor that are necessarily infringed by their
      Contribution(s) alone or by combination of their Contribution(s)
      with the Work to which such Contribution(s) was submitted. If You
      institute patent litigation against any entity (including a
      cross-claim or counterclaim in a lawsuit) alleging that the Work
      or a Contribution incorporated within the Work constitutes direct
      or contributory patent infringement, then any patent licenses
      granted to You under this License for that Work shall terminate
      as of the date such litigation is filed.

   4. Redistribution. You may reproduce and distribute copies of the
      Work or Derivative Works thereof in any medium, with or without
      modifications, and in Source or Object form, provided that You
      meet the following conditions:

      (a) You must give any other recipients of the Work or
          Derivative Works a copy of this License; and

      (b) You must cause any modified files to carry prominent notices
          stating that You changed the files; and

      (c) You must retain, in the Source form of any Derivative Works
          that You distribute, all copyright, patent, trademark, and
          attribution notices from the Source form of the Work,
          excluding those notices that do not pertain to any part of
          the Derivative Works; and

      (d) If the Work includes a "NOTICE" text file as part of its
          distribution, then any Derivative Works that You distribute must
          include a readable copy of the attribution notices contained
          within such NOTICE file, excluding those notices that do not
          pertain to any part of the Derivative Works, in at least one
          of the following places: within a NOTICE text file distributed
          as part of the Derivative Works; within the Source form or
          documentation, if provided along with the Derivative Works; or,
          within a display generated by the Derivative Works, if and
          wherever such third-party notices normally appear. The contents
          of the NOTICE file are for informational purposes only and
          do not modify the License. You may add Your own attribution
          notices within Derivative Works that You distribute, alongside
          or as an addendum to the NOTICE text from the Work, provided
          that such additional attribution notices cannot be construed
          as modifying the License.

      You may add Your own copyright statement to Your modifications and
      may provide additional or different license terms and conditions
      for use, reproduction, or distribution of Your modifications, or
      for any such Derivative Works as a whole, provided Your use,
      reproduction, and distribution of the Work otherwise complies with
      the conditions stated in this License.

   5. Submission of Contributions. Unless You explicitly state otherwise,
      any Contribution intentionally submitted for inclusion in the Work
      by You to the Licensor shall be under the terms and conditions of
      this License, without any additional terms or conditions.
      Notwithstanding the above, nothing herein shall supersede or modify
      the terms of any separate license agreement you may have executed
      with Licensor regarding such Contributions.

   6. Trademarks. This License does not grant permission to use the trade
      names, trademarks, service marks, or product names of the Licensor,
      except as required for reasonable and customary use in describing the
      origin of the Work and reproducing the content of the NOTICE file.

   7. Disclaimer of Warranty. Unless required by applicable law or
      agreed to in writing, Licensor provides the Work (and each
      Contributor provides its Contributions) on an "AS IS" BASIS,
      WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
      implied, including, without limitation, any warranties or conditions
      of TITLE, NON-INFRINGEMENT, MERCHANTABILITY, or FITNESS FOR A
      PARTICULAR PURPOSE. You are solely responsible for determining the
      appropriateness of using or redistributing the Work and assume any
      risks associated with Your exercise of permissions under this License.

   8. Limitation of Liability. In no event and under no legal theory,
      whether in tort (including negligence), contract, or otherwise,
      unless required by applicable law (such as deliberate and grossly
      negligent acts) or agreed to in writing, shall any Contributor be
      liable to You for damages, including any direct, indirect, special,
      incidental, or consequential damages of any character arising as a
      result of this License or out of the use or inability to use the
      Work (including but not limited to damages for loss of goodwill,
      work stoppage, computer failure or malfunction, or any and all
      other commercial damages or losses), even if such Contributor
      has been advised of the possibility of such damages.

   9. Accepting Warranty or Additional Liability. While redistributing
      the Work or Derivative Works thereof, You may choose to offer,
      and charge a fee for, acceptance of support, warranty, indemnity,
      or other liability obligations and/or rights consistent with this
      License. However, in accepting such obligations, You may act only
      on Your own behalf and on Your sole responsibility, not on behalf
      of any other Contributor, and only if You agree to indemnify,
      defend, and hold each Contributor harmless for any liability
      incurred by, or claims asserted against, such Contributor by reason
      of your accepting any such warranty or additional liability.

   END OF TERMS AND CONDITIONS

   APPENDIX: How to apply the Apache License to your work.

      To apply the Apache License to your work, attach the following
      boilerplate notice, with the fields enclosed by brackets "[]"
      replaced with your own identifying information. (Don't include
      the brackets!)  The text should be enclosed in the appropriate
      comment syntax for the file format. We also recommend that a
      file or class name and description of purpose be included on the
      same "printed page" as the copyright notice for easier
      identification within third-party archives.

   Copyright 2019 Akihiro Yamamoto.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License."""
