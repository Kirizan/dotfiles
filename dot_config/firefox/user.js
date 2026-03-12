// Firefox user preferences - deployed to profile directory by chezmoi
// These are re-applied on every Firefox startup

// Disable tab groups
user_pref("browser.tabs.groups.enabled", false);
user_pref("browser.tabs.groups.smart.enabled", false);
user_pref("browser.tabs.groups.smart.optin", false);
user_pref("browser.tabs.groups.smart.userEnabled", false);

// Disable "Choose a chatbot" and AI feature prompts
user_pref("browser.ml.chat.enabled", false);
user_pref("browser.ml.chat.sidebar", false);
user_pref("browser.ml.chat.prompt.prefix", "");

// Disable Mozilla feature recommendations and promotional popups
user_pref("browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons", false);
user_pref("browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features", false);
user_pref("browser.preferences.moreFromMozilla", false);
user_pref("browser.aboutwelcome.enabled", false);
user_pref("messaging-system.askForFeedback", false);

// Enable PipeWire for WebRTC media capture (microphone/camera on Wayland)
user_pref("media.webrtc.camera.allow-pipewire", true);
user_pref("media.navigator.mediadatadecoder_vpx_enabled", true);

// Enable audio output device selection (required for Teams speaker detection)
user_pref("media.setsinkid.enabled", true);
