using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Animation;
using System.Windows.Shapes;

namespace Silverlight
{
    public partial class MainPage : UserControl
    {
        private V6FS.Entry root;

        public MainPage()
        {
            InitializeComponent();
        }

        private void btnOpen_Click(object sender, RoutedEventArgs e)
        {
            var ofd = new OpenFileDialog();
            if (ofd.ShowDialog() != true) return;

            var sw = new StringWriter();
#if !DEBUG
            try
#endif
            {
                using (var fs = ofd.File.OpenRead())
                    root = V6FS.Open(fs);

                root.FileSystem.Write(sw);
                Action<V6FS.Entry> dir = null;
                dir = ent =>
                {
                    sw.WriteLine();
                    ent.Write(sw);
                    foreach (var child in ent.Children)
                        dir(child);
                };
                dir(root);
                btnSaveZip.IsEnabled = true;
            }
#if !DEBUG
            catch (Exception ex)
            {
                sw.WriteLine(ex.ToString());
                btnSaveZip.IsEnabled = false;
                root = null;
            }
#endif
            textBox1.Text = sw.ToString();
        }

        private void btnSaveZip_Click(object sender, RoutedEventArgs e)
        {
            var sfd = new SaveFileDialog();
            sfd.Filter = "Zip ファイル (*.zip)|*.zip|すべてのファイル (*.*)|*.*";
            if (sfd.ShowDialog() != true) return;

#if !DEBUG
            try
#endif
            {
                using (var fs = sfd.OpenFile())
                    V6FS.SaveZip(fs, root);
            }
#if !DEBUG
            catch (Exception ex)
            {
                MessageBox.Show(ex.ToString());
            }
#endif
        }
    }
}
