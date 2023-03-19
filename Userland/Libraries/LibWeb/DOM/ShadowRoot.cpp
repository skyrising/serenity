/*
 * Copyright (c) 2020, the SerenityOS developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <LibWeb/DOM/Document.h>
#include <LibWeb/DOM/Event.h>
#include <LibWeb/DOM/ShadowRoot.h>
#include <LibWeb/DOMParsing/InnerHTML.h>
#include <LibWeb/Layout/BlockContainer.h>

namespace Web::DOM {

ShadowRoot::ShadowRoot(Document& document, Element& host, Bindings::ShadowRootMode mode)
    : DocumentFragment(document)
    , m_mode(mode)
{
    set_host(&host);
}

JS::ThrowCompletionOr<void> ShadowRoot::initialize(JS::Realm& realm)
{
    MUST_OR_THROW_OOM(Base::initialize(realm));
    set_prototype(&Bindings::ensure_web_prototype<Bindings::ShadowRootPrototype>(realm, "ShadowRoot"));
    return {};
}

// https://dom.spec.whatwg.org/#ref-for-get-the-parent%E2%91%A6
EventTarget* ShadowRoot::get_parent(Event const& event)
{
    if (!event.composed()) {
        auto& events_first_invocation_target = verify_cast<Node>(*event.path().first().invocation_target);
        if (&events_first_invocation_target.root() == this)
            return nullptr;
    }

    return host();
}

// https://w3c.github.io/DOM-Parsing/#dom-innerhtml-innerhtml
WebIDL::ExceptionOr<DeprecatedString> ShadowRoot::inner_html() const
{
    return serialize_fragment(DOMParsing::RequireWellFormed::Yes);
}

// https://w3c.github.io/DOM-Parsing/#dom-innerhtml-innerhtml
WebIDL::ExceptionOr<void> ShadowRoot::set_inner_html(DeprecatedString const& markup)
{
    TRY(DOMParsing::inner_html_setter(*this, markup));

    set_needs_style_update(true);
    return {};
}

CSS::StyleSheetList& ShadowRoot::style_sheets()
{
    if (!m_style_sheets)
        m_style_sheets = CSS::StyleSheetList::create(document()).release_value_but_fixme_should_propagate_errors();
    return *m_style_sheets;
}

CSS::StyleSheetList const& ShadowRoot::style_sheets() const
{
    return const_cast<ShadowRoot*>(this)->style_sheets();
}

void ShadowRoot::visit_edges(Visitor& visitor)
{
    Base::visit_edges(visitor);
    visitor.visit(m_style_sheets.ptr());
}

}
