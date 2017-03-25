{ pkgs, stdenv, fetchFromGitHub, pythonPackages, wrapGAppsHook
, gst_all_1
}:

pythonPackages.buildPythonApplication rec {
  name = "mopidy-${version}";

  version = "2.1.0";

  src = fetchFromGitHub {
    owner = "mopidy";
    repo = "mopidy";
    rev = "v${version}";
    sha256 = "0krq5fbscqxayyc4vxai7iwxm2kdbgs5jicrdb013v04phw2za06";
  };

  nativeBuildInputs = [ wrapGAppsHook ];

  buildInputs = with pkgs; [
    pkgs.gstreamer gst-plugins-base gst-plugins-ugly gst-plugins-good
  ];

  propagatedBuildInputs = with pythonPackages; with pkgs; [
    pkgs.gst-python pygobject3 pykka tornado requests2 dbus-python
  ];

  # There are no tests
  doCheck = false;

  preFixup = ''
    gappsWrapperArgs+=(--prefix GST_PLUGIN_SYSTEM_PATH : "$GST_PLUGIN_SYSTEM_PATH")
  '';

  meta = with stdenv.lib; {
    homepage = http://www.mopidy.com/;
    description = ''
      An extensible music server that plays music from local disk, Spotify,
      SoundCloud, Google Play Music, and more
    '';
    license = licenses.asl20;
  };
}
