(** [ppx_browser] is a ppx which allows users to interact with the Browser-specific type
    [Browser_js_types.unsafe]. The ppx expression performs runtime checks and then
    executes the expression contained within if the runtime checks pass. The expression
    itself returns a [(a, b) result].

    A path argument is required, and is used to specify which context to check.

    For example: [ [%browser.Secure obj##!a] ]

    Calls the unsafe method [a] on [obj] if we're in a secure context.

    The extension nodes [##!] [##!.] and [new%js_unsafe] only exist within the ppx browser
    expression block, and are only usable on [Browser_js_types.unsafe] values. For all
    other values, use the regular [ppx_js] expressions

    Unsafe values are only able to be safely interacted with through the ppx, as otherwise
    we lose the JavaScript [this] binding.

    In order to use this ppx, you must add one of the following lines, depending on which
    library you are using

    {[
      open Browser.For_open
      open Browser_expert.For_open
      open Browser_unsafe_and_very_slow.For_open
      open Browser_expert_unsafe_and_very_slow.For_open
    ]} *)
